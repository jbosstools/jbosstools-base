package org.jboss.tools.runtime.as.ui.bot.test.detector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.RuntimeDetectionPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SeamPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.ServerRuntimesPreferencesDialog;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Runtime detection of JBoss server containing also Seam runtime. Checks if the JBoss AS and 
 * Seam can be added independently.  
 *  
 * @author Lucia Jelinkova
 *
 */
public class ServerWithSeam extends SWTTestExt {

	private RuntimeDetectionPreferencesDialog runtimeDetectionPreferences;

	private SearchingForRuntimesDialog searchingForRuntimesDialog;
	
	private SeamPreferencesDialog seamPreferences = new SeamPreferencesDialog();
	
	private ServerRuntimesPreferencesDialog serverRuntimesPreferences = new ServerRuntimesPreferencesDialog();
	
	@Before
	public void search(){
		runtimeDetectionPreferences = new RuntimeDetectionPreferencesDialog();
		runtimeDetectionPreferences.open();
		runtimeDetectionPreferences.addPath(RuntimeProperties.getInstance().getRuntimePath(DetectEAP5.SERVER_ID));
		searchingForRuntimesDialog = runtimeDetectionPreferences.search();
	}
	
	@Test
	public void serverFirst(){
		deselectRuntime("seam");
		
		assertSeamRuntimesNumber(0);
		assertServerRuntimesNumber(1);
		
		addAllDetectedRuntimes();
		
		assertSeamRuntimesNumber(1);
		assertServerRuntimesNumber(1);
	}
	
	@Test
	public void seamFirst(){
		deselectRuntime(DetectEAP5.SERVER_ID);
		
		assertSeamRuntimesNumber(1);
		assertServerRuntimesNumber(0);
		
		addAllDetectedRuntimes();
		
		assertSeamRuntimesNumber(1);
		assertServerRuntimesNumber(1);	
	}
	
	@After
	public void cleanup(){
		runtimeDetectionPreferences.open();
		runtimeDetectionPreferences.removePath(RuntimeProperties.getInstance().getRuntimePath(DetectEAP5.SERVER_ID));
		runtimeDetectionPreferences.ok();
		
		seamPreferences.open();
		seamPreferences.removeAllRuntimes();
		seamPreferences.ok();
		
		serverRuntimesPreferences.open();
		serverRuntimesPreferences.removeAllRuntimes();
		serverRuntimesPreferences.ok();
	}

	private void deselectRuntime(String name) {
		searchingForRuntimesDialog.deselect(name);
		searchingForRuntimesDialog.ok();
		runtimeDetectionPreferences.ok();
	}
	
	private void assertSeamRuntimesNumber(int expected) {
		seamPreferences.open();
		assertThat(seamPreferences.getRuntimes().size(), is(expected));
		seamPreferences.ok();
	}
	
	private void assertServerRuntimesNumber(int expected) {
		serverRuntimesPreferences.open();
		assertThat(serverRuntimesPreferences.getRuntimes().size(), is(expected));
		serverRuntimesPreferences.ok();
	}
	
	private void addAllDetectedRuntimes() {
		runtimeDetectionPreferences.open();
		searchingForRuntimesDialog = runtimeDetectionPreferences.search();
		searchingForRuntimesDialog.ok();
		runtimeDetectionPreferences.ok();
	}
}
