package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;

/**
 * Returns true while the specified shell is active.  
 * 
 * @author Lucia Jelinkova
 *
 */
public class ShellIsActiveCondition implements ICondition {

	private SWTBotShell shell;
	
	public ShellIsActiveCondition(SWTBotShell shell) {
		super();
		this.shell = shell;
	}

	@Override
	public void init(SWTBot bot) {
	}
	
	@Override
	public boolean test() throws Exception {
		return shell.isActive();
	}
	
	@Override
	public String getFailureMessage() {
		return "Expected the shell to become non active: " + shell;
	}
}
