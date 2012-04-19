package org.jboss.tools.runtime.as.ui.bot.test.jboss71;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.RuntimeDetectionPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;
import org.jboss.tools.runtime.as.ui.bot.test.matcher.ServerMatcher;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.After;
import org.junit.Test;

public class DetectJBoss71 extends SWTTestExt {

	public static final String JBOSS_7_1 = "jboss-as-7.1.0.Final";
	
	private RuntimeDetectionPreferencesDialog preferences;
	
	private SearchingForRuntimesDialog searchingForRuntimesDialog;
	
	@Test
	public void detectJBoss71(){
		preferences = new RuntimeDetectionPreferencesDialog();
		preferences.open();
		preferences.addPath(RuntimeProperties.getInstance().getRuntimePath(JBOSS_7_1));
		searchingForRuntimesDialog = preferences.search();
		
		assertThat(searchingForRuntimesDialog.getServers().size(), is(1));
		assertThat(searchingForRuntimesDialog.getServers().get(0), new ServerMatcher(getExpectedServer()));
	}
	
	private Server getExpectedServer() {
		Server expectedServer = new Server();
		expectedServer.setName(JBOSS_7_1);
		expectedServer.setVersion("7.1");
		expectedServer.setType("AS");
		expectedServer.setLocation(RuntimeProperties.getInstance().getRuntimePath(JBOSS_7_1));
		return expectedServer;
	}

	@After
	public void closePreferences(){
		searchingForRuntimesDialog.ok();
		preferences.ok();
	}
}
