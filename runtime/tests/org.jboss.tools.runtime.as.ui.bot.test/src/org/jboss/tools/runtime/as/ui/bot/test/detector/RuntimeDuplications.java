package org.jboss.tools.runtime.as.ui.bot.test.detector;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.jboss.tools.common.util.FileUtil;
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
		File tmpDir = new File(System.getProperty("java.io.tmpdir"));
		tmpServerPath = new File(tmpDir, "tmpServerCopy_" + System.currentTimeMillis());
		tmpServerAPath = new File(tmpServerPath, "serverA/jboss-eap-5.1");
		tmpServerBPath = new File(tmpServerPath, "serverB/jboss-eap-5.1");
		
		File server = new File(RuntimeProperties.getInstance().getRuntimePath(DetectEAP5.SERVER_ID));
		FileUtil.copyDir(server, tmpServerAPath, true, true, true);
		FileUtil.copyDir(server, tmpServerBPath, true, true, true);
	}
	
	@Test
	public void duplicateRuntimes(){
		searchingForRuntimesDialog = addPath(tmpServerPath.getAbsolutePath());
		assertFoundRuntimesNumber(4);
		
		searchingForRuntimesDialog = searchFirstPath();
		searchingForRuntimesDialog.ok();
		assertSeamRuntimesNumber(2);
		assertServerRuntimesNumber(2);
		
		searchingForRuntimesDialog = searchFirstPath();
		assertFoundRuntimesNumber(4);
		
		searchingForRuntimesDialog = searchFirstPath();
		searchingForRuntimesDialog.hideAlreadyCreatedRuntimes();
		assertFoundRuntimesNumber(0);
	}

	@After
	public void deleteServers() throws IOException{
		FileUtil.remove(tmpServerPath);
		
		removeAllPaths();
		removeAllSeamRuntimes();
		removeAllServerRuntimes();
	}
	
	private void assertFoundRuntimesNumber(int expected) {
		List<Runtime> runtimes = searchingForRuntimesDialog.getRuntimes();
		searchingForRuntimesDialog.cancel();
		assertThat(runtimes.size(), is(expected));
	}
}
