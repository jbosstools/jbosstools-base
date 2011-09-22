package org.jboss.tools.ui.bot.ext.helper;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

/**
 * Heper class for project imports
 */
public class ImportHelper {

	/**
	 * Import all projects from from given path to current workspace
	 */
	public static void importAllProjects(String path) {
		SWTBotExt bot = new SWTBotExt();
		bot.menu("File").menu("Import...").click();
		
		SWTBot dlgBot = bot.shell("Import").activate().bot();
		dlgBot.tree().expandNode("General").expandNode("Existing Projects into Workspace").select();
		dlgBot.button(IDELabel.Button.NEXT).click();
		
		dlgBot.radio(0).click();
		dlgBot.text().setText(path);
		dlgBot.radio(1).click();
		dlgBot.radio(0).click();
		dlgBot.button("Select All").click();
		dlgBot.button(IDELabel.Button.FINISH).click();
	}
}
