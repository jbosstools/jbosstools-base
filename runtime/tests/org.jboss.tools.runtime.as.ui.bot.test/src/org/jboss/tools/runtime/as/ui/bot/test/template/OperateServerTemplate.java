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
	public void startJBoss71(){
		serversView.startServer(getServerName());

		assertNoException();
		assertServerState("Started");
	}

	@Test
	public void restartJBoss71(){
		serversView.restartServer(getServerName());

		assertNoException();
		assertServerState("Started");
	}

	@Test
	public void stopJBoss71(){
		serversView.stopServer(getServerName());

		assertNoException();
		assertServerState("Stopped");
	}

	@Test
	public void deleteJBoss71(){
		serversView.deleteServer(getServerName());

		assertFalse(serversView.serverExists(getServerName()));
	}

	private void assertNoException() {
		assertThat("Exception:", not(new ConsoleOutputMatcher()));
	}

	private void assertServerState(String state) {
		assertThat(serversView.getServerStatus(getServerName()), is(state));
	}
}
