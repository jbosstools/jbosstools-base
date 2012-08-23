package org.jboss.tools.runtime.as.ui.bot.test.template;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.RuntimeDetectionPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SeamPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.ServerRuntimesPreferencesDialog;
import org.jboss.tools.ui.bot.ext.SWTTestExt;

/**
 * Provides useful methods that can be used by its descendants. 
 * 
 * @author Lucia Jelinkova
 *
 */
public abstract class RuntimeDetectionTestCase extends SWTTestExt {

	protected RuntimeDetectionPreferencesDialog runtimeDetectionPreferences = new RuntimeDetectionPreferencesDialog();

	protected SeamPreferencesDialog seamPreferences = new SeamPreferencesDialog();
	
	protected ServerRuntimesPreferencesDialog serverRuntimesPreferences = new ServerRuntimesPreferencesDialog();
	
	protected SearchingForRuntimesDialog addPath(String path){
		runtimeDetectionPreferences = new RuntimeDetectionPreferencesDialog();
		runtimeDetectionPreferences.open();
		runtimeDetectionPreferences.addPath(path);
		return runtimeDetectionPreferences.search();
	}
	
	protected SearchingForRuntimesDialog searchFirstPath(){
		runtimeDetectionPreferences = new RuntimeDetectionPreferencesDialog();
		runtimeDetectionPreferences.open();
		return runtimeDetectionPreferences.search();
	}
	
	protected void removeAllPaths(){
		runtimeDetectionPreferences.open();
		runtimeDetectionPreferences.removeAllPaths();
		runtimeDetectionPreferences.ok();
	}
	
	protected void removeAllSeamRuntimes(){
		seamPreferences.open();
		System.err.println(seamPreferences.getRuntimes());
		seamPreferences.removeAllRuntimes();
		assertThat(seamPreferences.getRuntimes().size(), is(0));
		seamPreferences.ok();
	}
	
	protected void removeAllServerRuntimes(){
		serverRuntimesPreferences.open();
		serverRuntimesPreferences.removeAllRuntimes();
		serverRuntimesPreferences.ok();
	}
	
	protected void assertSeamRuntimesNumber(int expected) {
		seamPreferences.open();
		assertThat(seamPreferences.getRuntimes().size(), is(expected));
		seamPreferences.ok();
	}
	
	protected void assertServerRuntimesNumber(int expected) {
		serverRuntimesPreferences.open();
		assertThat(serverRuntimesPreferences.getRuntimes().size(), is(expected));
		serverRuntimesPreferences.ok();
	}
}
