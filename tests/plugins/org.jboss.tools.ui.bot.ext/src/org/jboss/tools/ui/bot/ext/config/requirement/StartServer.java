package org.jboss.tools.ui.bot.ext.config.requirement;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

/**
 * Starts server (as dependent requirement has {@link AddServer}
 * 
 * @author lzoubek
 * 
 */
public class StartServer extends RequirementBase {

	public StartServer() {
		// define dependency
		getDependsOn().add(createAddServer());
	}

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getServer().isRunning;
	}

	@Override
	public void handle() {
		if (!checkFulfilled()) {
			String server = SWTTestExt.configuredState.getServer().name;
			String user = SWTTestExt.configuredState.getRemoteSystem().remoteUser;
			SWTTestExt.servers.startServer(server, user, null);
			SWTTestExt.configuredState.getServer().isRunning = true;
			// force console view not to steal focus when something happens on server
			try {
				SWTBotView consoleView = SWTTestExt.console.show();
				consoleView
						.toolbarToggleButton(
								IDELabel.ConsoleView.BUTTON_SHOW_WHEN_STDOUT_CHANGES_TOOLTIP)
						.deselect();
				consoleView
						.toolbarToggleButton(
								IDELabel.ConsoleView.BUTTON_SHOW_WHEN_STDERR_CHANGES_TOOLTIP)
						.deselect();
			} catch (Exception ex) {
			}
		}
	}
}
