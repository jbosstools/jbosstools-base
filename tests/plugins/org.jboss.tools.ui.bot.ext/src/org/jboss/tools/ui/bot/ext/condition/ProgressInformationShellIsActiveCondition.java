package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

/**
 * Checks if the progress information dialog is visible. 
 * 
 * @author Lucia Jelinkova
 *
 */
public class ProgressInformationShellIsActiveCondition implements ICondition {

	private static final String DIALOG_TITLE = "Progress Information";
	
	private SWTBotShell shell;

	@Override
	public void init(SWTBot bot) {
		try {
			shell = SWTBotFactory.getBot().shell("Progress Information");
		} catch (WidgetNotFoundException e){
			// nothing, dialog is not there
		}
	}
	
	@Override
	public boolean test() throws Exception {
		return shell != null && shell.isActive();
	}

	@Override
	public String getFailureMessage() {
		return "Expected the " + DIALOG_TITLE + " shell to become non active";
	}
}
