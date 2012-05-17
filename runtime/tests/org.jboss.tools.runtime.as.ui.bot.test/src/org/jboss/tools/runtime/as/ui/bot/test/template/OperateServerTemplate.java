package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.matcher.console.ConsoleOutputMatcher;
import org.jboss.tools.ui.bot.ext.view.ServersView;
import org.junit.Test;

/**
 * Checks if the given server can be started, restarted, stopped and deleted without
 * error. 
 * 
 * @author Lucia Jelinkova
 *
 */
public abstract class OperateServerTemplate extends SWTTestExt{ 

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
		serversView.startServer(getServerName());
		
		assertNoException("Starting server");
		assertServerState("Starting server", "Started");
		
	}

	public void restartServer(){
		serversView.restartServer(getServerName());

		assertNoException("Restarting server");
		assertServerState("Restarting server", "Started");
	}

	public void stopServer(){
		serversView.stopServer(getServerName());

		assertNoException("Stopping server");
		assertServerState("Stopping server", "Stopped");
	}

	public void deleteServer(){
		serversView.deleteServer(getServerName());

		assertFalse("Deleting server", serversView.serverExists(getServerName()));
	}

	protected void assertNoException(String message) {
		assertThat(message, "Exception:", not(new ConsoleOutputMatcher()));
	}

	protected void assertServerState(String message, String state) {
		assertThat(message, serversView.getServerStatus(getServerName()), is(state));
	}
}
