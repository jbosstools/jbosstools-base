package org.jboss.tools.runtime.as.ui.bot.test.detector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.RuntimeDetectionPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SeamPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.ServerRuntimesPreferencesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the case when the runtime path contains two identical server runtimes with Seam.  
 * 
 * @author Lucia Jelinkova
 *
 */
public class RuntimeDuplications extends SWTTestExt {

	private File tmpServerPath;
	
	private File tmpServerAPath;
	
	private File tmpServerBPath;
	
	private RuntimeDetectionPreferencesDialog runtimeDetectionPreferences;

	private SearchingForRuntimesDialog searchingForRuntimesDialog;
	
	private SeamPreferencesDialog seamPreferences = new SeamPreferencesDialog();
	
	private ServerRuntimesPreferencesDialog serverRuntimesPreferences = new ServerRuntimesPreferencesDialog();
	
	@Before
	public void prepareServers() throws IOException{
		File tmpDir = FileUtils.getTempDirectory();
		tmpServerPath = new File(tmpDir, "tmpServerCopy_" + System.currentTimeMillis());
		tmpServerAPath = new File(tmpServerPath, "serverA");
		tmpServerBPath = new File(tmpServerPath, "serverB");
		
		File server = new File(RuntimeProperties.getInstance().getRuntimePath(DetectEAP5.SERVER_ID));
		FileUtils.copyDirectoryToDirectory(server, tmpServerAPath);
		FileUtils.copyDirectoryToDirectory(server, tmpServerBPath);
	}
	
	@Test
	public void duplicateRuntimes(){
		searchRuntimePath();
		
		assertFoundRuntimesNumber(4);
		assertSeamRuntimesNumber(2);
		assertServerRuntimesNumber(2);
		
		searchRuntimePath();
		assertFoundRuntimesNumber(0);
	}

	@After
	public void deleteServers() throws IOException{
		FileUtils.deleteDirectory(tmpServerPath);
		
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
	
	private void searchRuntimePath() {
		runtimeDetectionPreferences = new RuntimeDetectionPreferencesDialog();
		runtimeDetectionPreferences.open();
		runtimeDetectionPreferences.addPath(tmpServerPath.getAbsolutePath());
		searchingForRuntimesDialog = runtimeDetectionPreferences.search();
	}
	
	private void assertFoundRuntimesNumber(int expected) {
		List<Runtime> runtimes = searchingForRuntimesDialog.getRuntimes();
		assertThat(runtimes.size(), is(expected));
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
}
