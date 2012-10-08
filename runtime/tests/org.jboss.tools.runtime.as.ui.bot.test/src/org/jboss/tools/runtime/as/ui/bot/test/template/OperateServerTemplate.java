package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import org.jboss.reddeer.eclipse.ui.console.ConsoleView;
import org.jboss.reddeer.eclipse.wst.server.ui.view.ServersView;
import org.junit.Test;

/**
 * Checks if the given server can be started, restarted, stopped and deleted without
 * error. 
 * 
 * @author Lucia Jelinkova
 *
 */
public abstract class OperateServerTemplate { 

	private ServersView serversView = new ServersView();

	protected abstract String getServerName();

	@Test
	public void operateServer(){
		startServer();
		restartServer();
		stopServer();
		deleteServer();
	}
	
	public void startServer(){
		serversView.getServer(getServerName()).start();
		
		assertNoException("Starting server");
		assertServerState("Starting server", "Started");
		
	}

	public void restartServer(){
		serversView.getServer(getServerName()).restart();

		assertNoException("Restarting server");
		assertServerState("Restarting server", "Started");
	}

	public void stopServer(){
		serversView.getServer(getServerName()).stop();

		assertNoException("Stopping server");
		assertServerState("Stopping server", "Stopped");
	}

	public void deleteServer(){
		serversView.getServer(getServerName()).delete();
	}

	protected void assertNoException(String message) {
		ConsoleView console = new ConsoleView();
		assertThat(message, console.getConsoleText(), not(containsString("Exception in thread")));
	}

	protected void assertServerState(String message, String state) {
		assertThat(message, serversView.getServer(getServerName()).getLabel().getState().getText(), is(state));
	}
}
