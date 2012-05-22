package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;

import java.util.List;

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

	protected abstract String getPathID();

	protected abstract List<Runtime> getExpectedRuntimes();

	@Test
	public void detectRuntime(){
		preferences = new RuntimeDetectionPreferencesDialog();
		preferences.open();
		preferences.addPath(RuntimeProperties.getInstance().getRuntimePath(getPathID()));
		searchingForRuntimesDialog = preferences.search();
		
		List<Runtime> runtimes = searchingForRuntimesDialog.getRuntimes(); 
		
		assertThat(runtimes.size(), is(getExpectedRuntimes().size()));
		for (Runtime runtime : getExpectedRuntimes()){
			assertThat(runtimes, hasItem(new RuntimeMatcher(runtime)));			
		}
	}

	@After
	public void closePreferences(){
		searchingForRuntimesDialog.ok();
		preferences.removePath(RuntimeProperties.getInstance().getRuntimePath(getPathID()));
		preferences.ok();
	}
}
