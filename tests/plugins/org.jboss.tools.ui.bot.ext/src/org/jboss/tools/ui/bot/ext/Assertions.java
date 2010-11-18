package org.jboss.tools.ui.bot.ext;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;

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
}
