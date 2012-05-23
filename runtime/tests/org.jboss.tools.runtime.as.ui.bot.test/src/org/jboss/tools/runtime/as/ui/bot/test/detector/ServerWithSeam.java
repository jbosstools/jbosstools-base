package org.jboss.tools.runtime.as.ui.bot.test.detector;

import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.template.RuntimeDetectionTestCase;
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
public class ServerWithSeam extends RuntimeDetectionTestCase {

	private SearchingForRuntimesDialog searchingForRuntimesDialog;
	
	@Before
	public void search(){
		searchingForRuntimesDialog = addPath(RuntimeProperties.getInstance().getRuntimePath(DetectEAP5.SERVER_ID));
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
		removeAllPaths();
		removeAllSeamRuntimes();
		removeAllServerRuntimes();
	}

	private void deselectRuntime(String name) {
		searchingForRuntimesDialog.deselect(name);
		searchingForRuntimesDialog.ok();
		runtimeDetectionPreferences.ok();
	}
	
	private void addAllDetectedRuntimes() {
		searchFirstPath();
		searchingForRuntimesDialog.ok();
		runtimeDetectionPreferences.ok();
	}
}
