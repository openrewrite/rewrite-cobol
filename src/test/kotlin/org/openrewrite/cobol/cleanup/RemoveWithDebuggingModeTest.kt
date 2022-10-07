package org.openrewrite.cobol.search

import org.junit.jupiter.api.Test
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.cleanup.RemoveWithDebuggingMode
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.test.RecipeSpec

class RemoveWithDebuggingModeTest : CobolTest() {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(RemoveWithDebuggingMode())
    }

    @Test
    fun noChange() = rewriteRun(
        cobolCopy(getNistSource("CM101M.CBL"))
    )

    @Test
    fun removeDebuggingMode() = rewriteRun(
        cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     CONTINUED.                                                   DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082                                                     SHIFTED
                000800         WITH DEBUGGING MODE.                                     DB1014.2
                000900 OBJECT-COMPUTER.                                                 DB1014.2
                001000     XXXXX083.                                                    DB1014.2
            """.trimIndent(),
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     CONTINUED.                                                   DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082.                                                    SHIFTED
                000800 OBJECT-COMPUTER.                                                 DB1014.2
                000900     XXXXX083.                                                    DB1014.2
            """.trimIndent())
    )

    @Test
    fun requiresAutoFormat() = rewriteRun(
        cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         
                000200 PROGRAM-ID.                                                      
                000300     CONTINUED.                                                   
                000400 ENVIRONMENT DIVISION.                                            
                000500 CONFIGURATION SECTION.                                           
                000600 SOURCE-COMPUTER.                                                 
                000700                                                          XXXXX082SHIFTED
                000800         WITH                                                     
                000900         DEBUGGING                                                
                001000         M                                                        
                001100-         O                                                       
                001200-          D                                                      
                001300-           E.                                                    
            """.trimIndent(),
            """
                000100 IDENTIFICATION DIVISION.                                         
                000200 PROGRAM-ID.                                                      
                000300     CONTINUED.                                                   
                000400 ENVIRONMENT DIVISION.                                            
                000500 CONFIGURATION SECTION.                                           
                000600 SOURCE-COMPUTER.                                                 
                000700                                                          XXXXX082SHIFTED
                000800                                                                  
                000900                                                                  
                001000                                                                  
                001100                                                                  
                001200                                                                  
                001300             .                                                    
            """.trimIndent())
    )

    @Test
    fun endOfCompilationUnit() = rewriteRun(
        cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         
                000200 PROGRAM-ID.                                                      
                000300     CONTINUED.                                                   
                000400 ENVIRONMENT DIVISION.                                            
                000500 CONFIGURATION SECTION.                                           
                000600 SOURCE-COMPUTER.                                                 
                000700     XXXXX082                                                     SHIFTED
                000800         WITH                                                     
                000900         DEBUGGING                                                
                001000         M                                                        
                001100-         O                                                       
                001200-          D                                                      
                001300-           E.                                                    
            """.trimIndent(), "" +
                    "000100 IDENTIFICATION DIVISION.                                         \n" +
                    "000200 PROGRAM-ID.                                                      \n" +
                    "000300     CONTINUED.                                                   \n" +
                    "000400 ENVIRONMENT DIVISION.                                            \n" +
                    "000500 CONFIGURATION SECTION.                                           \n" +
                    "000600 SOURCE-COMPUTER.                                                 \n" +
                    "000700     XXXXX082.                                                    SHIFTED\n\n"
        )
    )

    @Test
    fun isContinued() = rewriteRun(
        cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         
                000200 PROGRAM-ID.                                                      
                000300     CONTINUED.                                                   
                000400 ENVIRONMENT DIVISION.                                            
                000500 CONFIGURATION SECTION.                                           
                000600 SOURCE-COMPUTER.                                                 
                000700     XXXXX082                                                     SHIFTED
                000800         WITH                                                     
                000900         DEBUGGING                                                
                001000         M                                                        
                001100-         O                                                       
                001200-          D                                                      
                001300-           E.                                                    
                001400 OBJECT-COMPUTER.                                                 
                001500     XXXXX083.                                                    
            """.trimIndent(),
            """
                000100 IDENTIFICATION DIVISION.                                         
                000200 PROGRAM-ID.                                                      
                000300     CONTINUED.                                                   
                000400 ENVIRONMENT DIVISION.                                            
                000500 CONFIGURATION SECTION.                                           
                000600 SOURCE-COMPUTER.                                                 
                000700     XXXXX082.                                                    SHIFTED
                000800 OBJECT-COMPUTER.                                                 
                000900     XXXXX083.                                                    
            """.trimIndent())
    )
}