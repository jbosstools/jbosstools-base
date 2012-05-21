package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.RuntimeDetectionPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.matcher.RuntimeMatcher;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.After;
import org.junit.Test;

/**
 * Common scenario for runtime detection tests. It adds the runtime's installation
 * folder to the runtime detection and checks if it is correctly recognized and created. 
 *   
 * @author Lucia Jelinkova
 *
 */
public abstract class DetectRuntimeTemplate extends SWTTestExt {

	private RuntimeDetectionPreferencesDialog preferences;

	private SearchingForRuntimesDialog searchingForRuntimesDialog;

	protected abstract String getServerRuntimeID();

	protected abstract Runtime getExpectedServerRuntime();

	@Test
	public void detectRuntime(){
		preferences = new RuntimeDetectionPreferencesDialog();
		preferences.open();
		preferences.addPath(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		searchingForRuntimesDialog = preferences.search();
		
		assertThat(searchingForRuntimesDialog.getRuntimes().size(), is(1));
		assertThat(searchingForRuntimesDialog.getRuntimes().get(0), new RuntimeMatcher(getExpectedServerRuntime()));
	}

	@After
	public void closePreferences(){
		searchingForRuntimesDialog.ok();
		preferences.removePath(RuntimeProperties.getInstance().getRuntimePath(getServerRuntimeID()));
		preferences.ok();
	}
}
