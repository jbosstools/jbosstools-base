package org.jboss.tools.runtime.test.download;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.runtime.core.internal.RuntimeExtensionManager;
import org.jboss.tools.runtime.core.model.DownloadRuntime;

public class DownloadRuntimesTest extends TestCase {

	public void testDownloadRuntimes1() {
		DownloadTestProvider.setMode(DownloadTestProvider.MODE_NULL);
		Map<String, DownloadRuntime> map = new HashMap<String, DownloadRuntime>();
		RuntimeExtensionManager.getDefault().loadDownloadableRuntimesFromProviders(map, new NullProgressMonitor());
		assertEquals(map.size(), 0);
		
		map.clear();
		DownloadTestProvider.setMode(DownloadTestProvider.MODE_EMPTY);
		RuntimeExtensionManager.getDefault().loadDownloadableRuntimesFromProviders(map, new NullProgressMonitor());
		assertEquals(map.size(), 0);

		map.clear();
		DownloadTestProvider.setMode(DownloadTestProvider.MODE_TWO_ELEMENTS);
		RuntimeExtensionManager.getDefault().loadDownloadableRuntimesFromProviders(map, new NullProgressMonitor());
		assertEquals(map.size(), 2);
		
		assertTrue(map.get("id1").getName().equals("name1"));
		assertTrue(map.get("id2").getName().equals("name2"));
	}
}
