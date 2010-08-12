package org.jboss.tools.ui.bot.ext.config.requirement;
import org.jboss.tools.ui.bot.ext.SWTTestExt;

public class ClearWorkspace extends RequirementBase {
	// this requirement will always run handle();
	private boolean once = false;
	@Override
	public boolean checkFulfilled() {
		return once;
	}

	@Override
	public void handle() {
		SWTTestExt.bot.closeAllShells();
		SWTTestExt.bot.closeAllEditors();
		once = true;
	}

}
