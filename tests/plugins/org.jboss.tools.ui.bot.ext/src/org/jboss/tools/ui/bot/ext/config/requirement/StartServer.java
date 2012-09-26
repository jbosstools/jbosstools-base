package org.jboss.tools.ui.bot.ext.config.requirement;

import java.util.List;

import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarToggleButton;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.view.ConsoleView;

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
				ConsoleView consoleView = new ConsoleView();
				consoleView.show();
				List<SWTBotToolbarButton> buttons = consoleView.getToolbarButtons();
				for (SWTBotToolbarButton button : buttons) {
					if (button.getToolTipText().
							equals(IDELabel.ConsoleView.BUTTON_SHOW_WHEN_STDOUT_CHANGES_TOOLTIP)) {
						SWTBotToolbarToggleButton toggleButton = (SWTBotToolbarToggleButton) button;
						toggleButton.deselect();
						break;
					}
				}
				for (SWTBotToolbarButton button : buttons) {
					if (button.getToolTipText().
							equals(IDELabel.ConsoleView.BUTTON_SHOW_WHEN_STDERR_CHANGES_TOOLTIP)) {
						SWTBotToolbarToggleButton toggleButton = (SWTBotToolbarToggleButton) button;
						toggleButton.deselect();
						break;
					}
				}
			} catch (Exception ex) {
			}
		}
	}
}
