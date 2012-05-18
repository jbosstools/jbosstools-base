package org.jboss.tools.runtime.as.ui.bot.test.template;

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

/**
 * Common scenario for server detection tests. It adds the server's installation
 * folder to the runtime detection and checks if it is correctly recognized and created. 
 *   
 * @author Lucia Jelinkova
 *
 */
public abstract class DetectServerTemplate extends SWTTestExt {

	private RuntimeDetectionPreferencesDialog preferences;

	private SearchingForRuntimesDialog searchingForRuntimesDialog;

	protected abstract String getServerID();

	protected abstract Server getExpectedServer();

	@Test
	public void detectServer(){
		preferences = new RuntimeDetectionPreferencesDialog();
		preferences.open();
		preferences.addPath(RuntimeProperties.getInstance().getRuntimePath(getServerID()));
		searchingForRuntimesDialog = preferences.search();
		
		assertThat(searchingForRuntimesDialog.getServers().size(), is(1));
		assertThat(searchingForRuntimesDialog.getServers().get(0), new ServerMatcher(getExpectedServer()));
	}

	@After
	public void closePreferences(){
		searchingForRuntimesDialog.ok();
		preferences.removePath(RuntimeProperties.getInstance().getRuntimePath(getServerID()));
		preferences.ok();
	}
}
