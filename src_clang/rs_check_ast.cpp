//------------------------------------------------------------------------------
// Clang rewriter sample. Demonstrates:
//
// * How to use RecursiveASTVisitor to find interesting AST nodes.
// * How to use the Rewriter API to rewrite the source code.
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//------------------------------------------------------------------------------
#include <cstdio>
#include <memory>
#include <string>
#include <sstream>

#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ASTConsumer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "llvm/Support/Host.h"

using namespace clang;

 
class ASTContext;

// Jiading Gai
class RSCheckAST : public clang::StmtVisitor<RSCheckAST> {
  private:
    // [Gai] slang::RSContext *Context;
    clang::ASTContext &C;
    // [Gai] clang::SourceManager &mSM;
    bool mValid;
    unsigned int mTargetAPI;
    bool mIsFilterscript;
    bool mInKernel;

    void WarnOnSetElementAt(clang::CallExpr*);

  public:
   // explicit RSCheckAST(RSContext &Con, unsigned int TargetAPI, 
   //                     bool IsFilterscript)
   //   : Context(Con),
   //     C(Con->getASTContext()),
   //     mSM(C.getSourceManager()),
   //     mValid(true),
   //     mTargetAPI(TargetAPI),
   //     mIsFilterscript(IsFilterscript),
   //     mInKernel(false) {
   //     
   // }

    explicit RSCheckAST(clang::ASTContext &Con, unsigned int TargetAPI,
                       bool IsFilterscript)
      : C(Con),
        mValid(true),
        mTargetAPI(TargetAPI),
        mIsFilterscript(IsFilterscript),
        mInKernel(false) {
    }

    void VisitStmt(clang::Stmt *S);
    void VisitCallExpr(clang::CallExpr *CE);
    void VisitCastExpr(clang::CastExpr *CE);
    void VisitExpr(clang::Expr *E);
    void VisitDeclStmt(clang::DeclStmt *DS);
    void ValidateFunctionDecl(clang::FunctionDecl *FD);
    void ValidateVarDecl(clang::VarDecl *VD);
    bool Validate();
};

void RSCheckAST::VisitStmt(clang::Stmt *S) {
  for (clang::Stmt::child_iterator I = S->child_begin(), E = S->child_end();
       I != E; ++I) {
    if (clang::Stmt *Child = *I) {
      Visit(Child);
    }
  }
}

void RSCheckAST::VisitCallExpr(clang::CallExpr *CE) {
  for (clang::CallExpr::arg_iterator AI = CE->arg_begin(), AE = CE->arg_end();
       AI != AE; ++AI) {
    Visit(*AI);
  }
}

void RSCheckAST::VisitCastExpr(clang::CastExpr *CE) {
  if (CE->getCastKind() == clang::CK_BitCast) {
    clang::QualType QT = CE->getType();
    const clang::Type *T = QT.getTypePtr();
    if (T->isVectorType()) {
      if (llvm::isa<clang::ImplicitCastExpr>(CE)) {
        llvm::errs() << "invalid implicit vector cast\n";
      } else {
        llvm::errs() << "invalid vector cast\n";
      }
      mValid = false; 
    }
  }
  Visit(CE->getSubExpr());
}

void RSCheckAST::VisitExpr(clang::Expr *E) {
  E->IgnoreImpCasts();
  if (mIsFilterscript /*&&
      !SlangRS::IsLocInRSHeaderFilt(E->getExprLoc(), mSM) &&
      !RSExportType::ValidateType(Context, C, E->getType(), nullptr, E->getExprLoc(),
                                  mTargetAPI, mIsFilterscript)*/) {
    mValid = false;
  } else {
    VisitStmt(E);
  }
}

void RSCheckAST::VisitDeclStmt(clang::DeclStmt *DS) {
  //if (!SlangRS::IsLocInRSHeaderFilt(DS->getLocStart(), mSM)) {
      for (clang::DeclStmt::decl_iterator I = DS->decl_begin(), 
                                          E = DS->decl_end();
           I != E;
           ++I) {
        if (clang::VarDecl *VD = llvm::dyn_cast<clang::VarDecl>(*I)) {
          ValidateVarDecl(VD);
        } else if (clang::FunctionDecl *FD = llvm::dyn_cast<clang::FunctionDecl>(*I)) {
          ValidateFunctionDecl(FD);
        }
      }
  //}
}

void RSCheckAST::ValidateFunctionDecl(clang::FunctionDecl *FD) 
{
  if (!FD) {
    return;
  }

  if (mIsFilterscript) {
    size_t numParams = FD->getNumParams();
    clang::QualType resultType = FD->getReturnType().getCanonicalType();

   // if (RSExportType::ValidateType(Context, C, resultType, FD,
   //                                FD->getLocStart(), mTargetAPI,
   //                                mIsFilterscript)) {
   //   mValid = false;
   // }

    for (size_t i = 0; i < numParams; i++) {
      clang::ParmVarDecl *PVD = FD->getParamDecl(i);
      clang::QualType QT = PVD->getType().getCanonicalType();

     // if (!RSExportType::ValidateType(Context, C, QT, PVD, PVD->getLocStart(),
     //                                 mTargetAPI, mIsFilterscript)) {
     //   mValid = false;
     // }
    }

  }

  bool  saveKernel = mInKernel;
  //mInKernel = RSExportForEach::isRSForEachFunc(mTargetAPI, Context, FD);

  if (clang::Stmt *Body = FD->getBody()) {
    Visit(Body);
  }

  mInKernel = saveKernel;
}

void RSCheckAST::ValidateVarDecl(clang::VarDecl *VD)
{
  if (!VD) {
    return;
  }

  clang::QualType QT = VD->getType();
  if (VD->getFormalLinkage() == clang::ExternalLinkage) {
    llvm::StringRef TypeName;
    const clang::Type *T = QT.getTypePtr();
   // if (!RSExportType::NormalizeType(T, TypeName, Context, VD)) {
   //   mValid = false;
   // }
  }

  if (mInKernel && VD->isStaticLocal()) {
    if (!QT.isConstQualified()) {
      llvm::errs() << "Non-const static variables are not allowed in kernels.\n";
      mValid = false;
    }
  }

  /*if (!RSExportType::ValidateVarDecl(Context, VD, mTargetAPI, mIsFilterscript)) {
    mValid = false;
  } else */if (clang::Expr *Init = VD->getInit()) {
    Visit(Init);
  }
}

bool RSCheckAST::Validate() 
{
  llvm::errs() << "[Gai]: Starting the renderscript AST validation.\n";
  clang::TranslationUnitDecl *TUDecl = C.getTranslationUnitDecl();
  for (clang::DeclContext::decl_iterator DI = TUDecl->decls_begin(), 
       DE = TUDecl->decls_end(); 
       DI != DE;
       DI++) {
    //if (!SlangRS::IsLocInRSHeaderFile(DI->getLocStart(), mSM)) {
        if (clang::VarDecl *VD = llvm::dyn_cast<clang::VarDecl>(*DI)) {
          ValidateVarDecl(VD);
        } else if (clang::FunctionDecl *FD = llvm::dyn_cast<clang::FunctionDecl>(*DI)) {
          ValidateFunctionDecl(FD);
        } else if (clang::Stmt *Body = (*DI)->getBody()) {
          Visit(Body);
        }
    //}
  }
}

// By implementing RecursiveASTVisitor, we can specify which AST nodes
// we're interested in by overriding relevant methods.
class MyASTVisitor : public RecursiveASTVisitor<MyASTVisitor> {
public:
  MyASTVisitor(Rewriter &R) : TheRewriter(R) {}

  bool VisitStmt(Stmt *s) {
    // Only care about If statements.
    if (isa<IfStmt>(s)) {
      IfStmt *IfStatement = cast<IfStmt>(s);
      Stmt *Then = IfStatement->getThen();

      TheRewriter.InsertText(Then->getLocStart(), "// the 'if' part\n", true,
                             true);

      Stmt *Else = IfStatement->getElse();
      if (Else)
        TheRewriter.InsertText(Else->getLocStart(), "// the 'else' part\n",
                               true, true);
    }

    return true;
  }

  bool VisitFunctionDecl(FunctionDecl *f) {
    // Only function definitions (with bodies), not declarations.
    llvm::errs() << "[Gai] " <<  f->getNameInfo() << "\n";
    if (f->hasBody()) {
      Stmt *FuncBody = f->getBody();

      // Type name as string
      QualType QT = f->getReturnType();
      std::string TypeStr = QT.getAsString();

      // Function name
      DeclarationName DeclName = f->getNameInfo().getName();
      std::string FuncName = DeclName.getAsString();

      // Add comment before
      std::stringstream SSBefore;
      SSBefore << "// Begin function " << FuncName << " returning " << TypeStr
               << "\n";
      SourceLocation ST = f->getSourceRange().getBegin();
      TheRewriter.InsertText(ST, SSBefore.str(), true, true);

      // And after
      std::stringstream SSAfter;
      SSAfter << "\n// End function " << FuncName;
      ST = FuncBody->getLocEnd().getLocWithOffset(1);
      TheRewriter.InsertText(ST, SSAfter.str(), true, true);
    }

    return true;
  }

private:
  Rewriter &TheRewriter;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : Visitor(R) {}

  // Override the method that gets called for each parsed top-level
  // declaration.
  virtual bool HandleTopLevelDecl(DeclGroupRef DR) {
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b)
      // Traverse the declaration using our AST visitor.
      Visitor.TraverseDecl(*b);
    return true;
  }

private:
  MyASTVisitor Visitor;
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    llvm::errs() << "Usage: rewritersample <filename>\n";
    return 1;
  }

  // CompilerInstance will hold the instance of the Clang compiler for us,
  // managing the various objects needed to run the compiler.
  CompilerInstance TheCompInst;
  TheCompInst.createDiagnostics();

  LangOptions &lo = TheCompInst.getLangOpts();
  lo.CPlusPlus = 1;

  // Initialize target info with the default triple for our platform.
  auto TO = std::make_shared<TargetOptions>();
  TO->Triple = llvm::sys::getDefaultTargetTriple();
  TargetInfo *TI =
      TargetInfo::CreateTargetInfo(TheCompInst.getDiagnostics(), TO);
  TheCompInst.setTarget(TI);

  TheCompInst.createFileManager();
  FileManager &FileMgr = TheCompInst.getFileManager();
  TheCompInst.createSourceManager(FileMgr);
  SourceManager &SourceMgr = TheCompInst.getSourceManager();
  TheCompInst.createPreprocessor(TU_Module);
  TheCompInst.createASTContext();

  // A Rewriter helps us manage the code rewriting task.
  Rewriter TheRewriter;
  TheRewriter.setSourceMgr(SourceMgr, TheCompInst.getLangOpts());

  // Set the main file handled by the source manager to the input file.
  const FileEntry *FileIn = FileMgr.getFile(argv[1]);
  SourceMgr.setMainFileID(
      SourceMgr.createFileID(FileIn, SourceLocation(), SrcMgr::C_User));
  TheCompInst.getDiagnosticClient().BeginSourceFile(
      TheCompInst.getLangOpts(), &TheCompInst.getPreprocessor());

  // Create an AST consumer instance which is going to get called by
  // ParseAST.
  MyASTConsumer TheConsumer(TheRewriter);

  // Parse the file to AST, registering our consumer as the AST consumer.
  ParseAST(TheCompInst.getPreprocessor(), &TheConsumer,
           TheCompInst.getASTContext());


  // Renderscript AST checker
  RSCheckAST rs_checker (TheCompInst.getASTContext(), 19, false);
  rs_checker.Validate();

  // At this point the rewriter's buffer should be full with the rewritten
  // file contents.
  const RewriteBuffer *RewriteBuf =
      TheRewriter.getRewriteBufferFor(SourceMgr.getMainFileID());
  llvm::outs() << std::string(RewriteBuf->begin(), RewriteBuf->end());

  return 0;
}
