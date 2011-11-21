package org.jboss.tools.ui.bot.ext;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.jboss.tools.ui.bot.ext.helper.MarkerHelper;

public class Assertions {
	/**
	 * asserts that given items are contained in tree (deep down)
	 * @param tree tree container
	 * @param items list of items that must be contatined
	 */
	public static void assertTreeContent(SWTBotTree tree, String... items) {
		assertTrue("Editor tree does not contain expected path of nodes : "
				+ Arrays.toString(items),
				SWTEclipseExt.containstInTree(tree, items));
	}
	/**
	 * asserts that given control is enabled/disabled
	 * @param control given control
	 * @param enabled state - true for enabled, false for disabled
	 */
	public static void assertControlEnabled(AbstractSWTBot<?> control, boolean enabled) {
		assertTrue(control.getClass().getSimpleName()+" "+control.getText()+ " is not in state: isEnabled()="+enabled,enabled == control.isEnabled());			
	}
	/**
	 * Asserts that given file exists
	 * @param fileName
	 * @param pathWithinWorkspace
	 */
	public static void assertFileExistsInWorkspace (String fileName , String... pathWithinWorkspace){
	  assertTrue("File " + fileName + " does not exist.",
	      isExistingFileWithinWorkspace (fileName, pathWithinWorkspace));
	}
	/**
   * Asserts that given file does not exists
   * @param fileName
   * @param pathWithinWorkspace
   */
  public static void assertFileNotExistsInWorkspace (String fileName , String... pathWithinWorkspace){
    assertTrue("File " + fileName + " does exist but it has to be deleted.",
        !isExistingFileWithinWorkspace (fileName, pathWithinWorkspace));
  }
	/**
	 * Returns true when specified file exists on the path within workspace
	 * @param fileName
	 * @param pathWithinWorkspace
	 * @return
	 */
	private static boolean isExistingFileWithinWorkspace(String fileName , String... pathWithinWorkspace){
	  return ResourcesPlugin
      .getWorkspace()
      .getRoot()
      .findMember(MarkerHelper.getPathToResource(fileName, pathWithinWorkspace)) != null;
	}
  /**
   * Asserts if sourceEditorText of file with fileName contains textToContain
   * @param sourceEditorText
   * @param textToContain
   * @param fileName
   */
  public static void assertSourceEditorContains (String sourceEditorText, String textToContain, String fileName){
    
    assertTrue("File " + fileName
        + " has to contain string '" 
        + textToContain
        + "' but it doesn't.\nIt is: " + sourceEditorText, 
        sourceEditorText.contains(textToContain));
    
  }
  /**
   * Asserts if sourceEditorText of file with fileName does not contain textToNotContain
   * @param sourceEditorText
   * @param textToNotContain
   * @param fileName
   */
  public static void assertSourceEditorNotContain (String sourceEditorText, String textToNotContain, String fileName){
    
    assertTrue("File " + fileName
        + " must not contain string '" 
        + textToNotContain
        + "' but it does.\nIt is: " + sourceEditorText, 
        !sourceEditorText.contains(textToNotContain));
    
  }
  /**
   * Asserts if sourceEditorText of file with fileName equals to expectedText
   * @param sourceEditorText
   * @param expectedText
   * @param fileName
   */
  public static void assertSourceEditorIs (String sourceEditorText, String expectedText, String fileName){
    
    assertTrue("File " + fileName
        + " has to be '" 
        + expectedText
        + "' but it doesn't.\nIt is: " + sourceEditorText, 
        sourceEditorText.equals(expectedText));
    
  }
}
