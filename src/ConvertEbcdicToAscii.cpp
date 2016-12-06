#include <sstream>
#include <string>

#include "clang/Driver/Options.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/FrontEnd/CompilerInstance.h"
#include "clang/FrontEnd/FrontEndActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Token.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Lex/MacroInfo.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;
using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
using namespace llvm;

static llvm::cl::OptionCategory MyToolCategory("ebcdic2ascii options");
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp("\nMore help text ...");

Rewriter rewriter;

static const unsigned int ebcdic2ascii[256] = {
  0x00, 0x01, 0x02, 0x03, 0xff, 0x09, 0xff, 0x7f, 0xff, 0xff, 0xff, 0x0b, 0x0c,
  0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0xff, 0x0a, 0x08, 0xff, 0x18, 0x19,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1c, 0xff, 0xff, 0x0a, 0x17,
  0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0x05, 0x06, 0x07, 0xff, 0xff, 0x16, 0xff,
  0xff, 0xff, 0xff, 0x04, 0x14, 0x15, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1a, 0x20,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x2e, 0x3c, 0x28,
  0x2b, 0x7c, 0x26, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x21,
  0x24, 0x2a, 0x29, 0x3b, 0x5e, 0x2d, 0x2f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0x2c, 0x25, 0x5f, 0x3e, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22, 0xff, 0x61,
  0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
  0x7a, 0xff, 0xff, 0xff, 0x5b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x5d, 0xff, 0xff, 0x7b, 0x41, 0x42,
  0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0x5c, 0xff, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
  0x37, 0x38, 0x39, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff 
  };

class ConvertStringsInMacros: public clang::PPCallbacks {
public:
   explicit ConvertStringsInMacros(SourceManager * SM) 
       : SM(SM) {}
   void MacroDefined (const Token & MacroNameTok,
                      const MacroDirective *MD) override {
      SourceLocation Loc  = SM->getExpansionLoc(MacroNameTok.getLocation());
      const MacroInfo * info = MD->getMacroInfo();
      if (SM->isInMainFile(Loc)) {
        unsigned num_tok = info->getNumTokens();
        ArrayRef<Token> tokens = info->tokens();

        //Find a stringification and/or literal tokens 
        for (unsigned x = 0 ; x < num_tok ; x++) {
            Token CurTok = tokens[x];
            if (CurTok.is(tok::hash)) {
                rewriter.InsertTextBefore(CurTok.getLocation(),"USTR(");
                traceMsg("Stringification at:", CurTok);
                rewriter.InsertTextAfterToken(tokens[x+1].getLocation(),")");
                }

            if (CurTok.is(tok::string_literal)) {
                rewriter.InsertTextBefore(CurTok.getLocation(),"u8");
                traceMsg("String Literal at:", CurTok);
                }

            if (CurTok.is(tok::char_constant)) {
                std::stringstream intToString;
                const char * character_lit = CurTok.getLiteralData();
                unsigned int lit_idx = 1;
                //skip escape backslash in escape_seq 
                if (character_lit[lit_idx] == '\\') {
                   lit_idx++;
                   } 
                intToString << "\'\\x" << std::hex << ebcdic2ascii[(unsigned int)character_lit[lit_idx]]<<"\'";
                rewriter.ReplaceText(CurTok.getLocation(),3, intToString.str());
                traceMsg("Char constant at:", CurTok);
                }
          }
        Loc.print(outs(), *SM);
        outs() << "Num Tokens:";
        outs() << num_tok << "\n";
      }
   }
private:
   SourceManager *const SM;
   void traceMsg(const char * msg, Token &tok) {
         SourceLocation Loc = tok.getLocation();
         outs() << msg;
         Loc.print(outs(), *SM);
         outs() << "\n";
      }


};

class LiteralVisitor: public RecursiveASTVisitor<LiteralVisitor> {
private:
   ASTContext * astContext;
public:
   explicit LiteralVisitor(CompilerInstance *CI) 
       : astContext(&(CI->getASTContext())) {
        rewriter.setSourceMgr(astContext->getSourceManager(),
        astContext->getLangOpts());
    }
    
    virtual bool VisitCharacterLiteral(CharacterLiteral *lit) {
      std::stringstream asciiEncoding;
      unsigned ascii_val = ebcdic2ascii[lit->getValue()];
      asciiEncoding << "\'\\x" << std::hex << ascii_val << "\'";
      rewriter.ReplaceText(SourceRange(lit->getLocStart(), lit->getLocEnd()), asciiEncoding.str());  
      return true;
    }
    
    virtual bool VisitStringLiteral(StringLiteral * lit) {
     rewriter.InsertTextBefore(lit->getLocStart(),"u8");
     return true;
    }
};

class StringLiteralConsumer : public ASTConsumer {
private:
  LiteralVisitor * visitor;
public:
  explicit StringLiteralConsumer(CompilerInstance * CI)
      : visitor(new LiteralVisitor(CI)) {}
  
  virtual void HandleTranslationUnit(ASTContext &context) override {
     visitor->TraverseDecl(context.getTranslationUnitDecl());
    }
};

class ConvertLiteralsAction: public ASTFrontendAction {
public:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) {
        CI.getPreprocessor().addPPCallbacks(llvm::make_unique<ConvertStringsInMacros>(&CI.getSourceManager()));
        return llvm::make_unique<StringLiteralConsumer>(&CI);
   }
};


//Convert all literals in source from EBCDIC to ASCII representation
int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  int result = Tool.run(newFrontendActionFactory<ConvertLiteralsAction>().get());
  rewriter.getEditBuffer(rewriter.getSourceMgr().getMainFileID()).write(outs());
  return result;
}
