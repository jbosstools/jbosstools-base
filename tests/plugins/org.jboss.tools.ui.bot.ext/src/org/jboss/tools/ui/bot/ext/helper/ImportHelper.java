package org.jboss.tools.ui.bot.ext.helper;

import static org.eclipse.swtbot.swt.finder.waits.Conditions.shellCloses;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
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
		int timeout = 100000; // 100s max timeout
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
		SWTBotShell s = dlgBot.activeShell();
		dlgBot.button(IDELabel.Button.FINISH).click();
		bot.waitUntil(shellCloses(s),timeout);
	}	
}
