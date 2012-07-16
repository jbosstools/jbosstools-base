package org.jboss.tools.ui.bot.ext.helper;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.osgi.framework.Bundle;

public class SubversiveHelper {
	
	static Logger log = Logger.getLogger(SubversiveHelper.class);

	/**
	 * Check if Subversive is installed or not
	 */
	public static boolean isSubversiveInstalled() {
		String[] names = { "org.eclipse.team.svn","org.eclipse.team.svn.core","org.eclipse.team.svn.ui"};
	
		boolean result = true;
		for (String name : names ) {
			Bundle bundle = Platform.getBundle(name);
			if (bundle == null) {
				result = false;
				break;
			}			
		}
		return result;			
	}
	
	/**
	 * Disables Subversive Decorations (for projects, etc.)
	 */
	public static void disableSVNDecoration() {

		if (!isSubversiveInstalled()) {
			log.info("Subversive is not installed, skiping SVN Decoration disabling");
			return;
		}
		
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
