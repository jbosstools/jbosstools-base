package org.jboss.tools.runtime.as.ui.bot.test.jboss71;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.matcher.console.ConsoleOutputMatcher;
import org.jboss.tools.ui.bot.ext.view.ServersView;
import org.junit.Test;

public class OperateJBoss71 extends SWTTestExt {

	private ServersView serversView = new ServersView();
	
	@Test
	public void startJBoss71(){
		serversView.startServer(DetectJBoss71.JBOSS_7_1);
		
		assertNoException();
		assertServerState("Started");
	}

	@Test
	public void restartJBoss71(){
		serversView.restartServer(DetectJBoss71.JBOSS_7_1);
		
		assertNoException();
		assertServerState("Started");
	}
	
	@Test
	public void stopJBoss71(){
		serversView.stopServer(DetectJBoss71.JBOSS_7_1);
		
		assertNoException();
		assertServerState("Stopped");
	}
	
	@Test
	public void deleteJBoss71(){
		serversView.deleteServer(DetectJBoss71.JBOSS_7_1);
		
		assertFalse(serversView.serverExists(DetectJBoss71.JBOSS_7_1));
	}
	
	private void assertNoException() {
		assertThat("Exception:", not(new ConsoleOutputMatcher()));
	}
	
	private void assertServerState(String state) {
		assertThat(serversView.getServerStatus(DetectJBoss71.JBOSS_7_1), is(state));
	}
}
