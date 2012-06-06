package org.jboss.tools.ui.bot.ext.helper;

import org.jboss.tools.ui.bot.ext.SWTBotExt;

public class SubversiveHelper {

	/**
	 * Disables Subversive Decorations (for projects, etc.)
	 */
	public static void disableSVNDecoration() {

		// commented, this is relevant only when SVN is installed as all

		SWTBotExt botExt = new SWTBotExt();
		botExt.menu("Window").menu("Preferences").click();
		botExt.tree().expandNode("Team", "SVN", "Label Decorations").select();

		botExt.tabItem("Text Decorations").activate();
		botExt.textWithLabel("File:").setText("");
		botExt.textWithLabel("Folder:").setText("");
		botExt.textWithLabel("Project:").setText("");
		botExt.button("OK").click();

		/*
		 * Obsolete text botExt.tabItem("Text").activate();
		 * botExt.textWithLabel("File Format:").setText("");
		 * botExt.textWithLabel("Folder Format:").setText("");
		 * botExt.textWithLabel("Project Format:").setText("");
		 * botExt.button("OK").click();
		 */
	}
}
