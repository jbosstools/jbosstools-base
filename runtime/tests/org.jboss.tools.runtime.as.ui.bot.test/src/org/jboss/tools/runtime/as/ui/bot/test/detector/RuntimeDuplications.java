package org.jboss.tools.runtime.as.ui.bot.test.detector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.jboss.tools.runtime.as.ui.bot.test.RuntimeProperties;
import org.jboss.tools.runtime.as.ui.bot.test.detector.server.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.dialog.preferences.SearchingForRuntimesDialog;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;
import org.jboss.tools.runtime.as.ui.bot.test.template.RuntimeDetectionTestCase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the case when the runtime path contains two identical server runtimes with Seam.  
 * 
 * @author Lucia Jelinkova
 *
 */
public class RuntimeDuplications extends RuntimeDetectionTestCase {

	private File tmpServerPath;
	
	private File tmpServerAPath;
	
	private File tmpServerBPath;
	
	private SearchingForRuntimesDialog searchingForRuntimesDialog;
	
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
		searchingForRuntimesDialog = addPath(tmpServerPath.getAbsolutePath());
		
		assertFoundRuntimesNumber(4);
		assertSeamRuntimesNumber(2);
		assertServerRuntimesNumber(2);
		
		searchFirstPath();
		assertFoundRuntimesNumber(0);
	}

	@After
	public void deleteServers() throws IOException{
		FileUtils.deleteDirectory(tmpServerPath);
		
		removeAllPaths();
		removeAllSeamRuntimes();
		removeAllServerRuntimes();
	}
	
	private void assertFoundRuntimesNumber(int expected) {
		List<Runtime> runtimes = searchingForRuntimesDialog.getRuntimes();
		assertThat(runtimes.size(), is(expected));
		searchingForRuntimesDialog.ok();
		runtimeDetectionPreferences.ok();
	}
}
