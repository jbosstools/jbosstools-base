/*************************************************************************************
 * Copyright (c) 2011 JBoss by Red Hat and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.test;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeType;
import org.eclipse.wst.server.core.ServerCore;
import org.jboss.ide.eclipse.as.core.util.IJBossToolingConstants;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.model.ServerDefinition;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.seam.core.project.facet.SeamRuntime;
import org.jboss.tools.seam.core.project.facet.SeamRuntimeManager;
import org.jboss.tools.seam.core.project.facet.SeamVersion;
import org.junit.Test;

/**
 * 
 * @author snjeza
 *
 */
public class RuntimeDetectionTest {

	@Test
	public void testRuntimeDetectors() {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getRuntimeDetectors();
		assertTrue("Runtime detectors don't exist.",detectors.size() > 0);
	}
	
	@Test
	public void testRuntimePaths() {
		List<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
		assertTrue("There are initial runtime paths.", runtimePaths.size() == 0);
	}
	
	@Test
	public void addRuntimePaths() {
		List<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
		String[] paths = { IRuntimeDetectionConstants.JBOSS_42_HOME, IRuntimeDetectionConstants.JBOSS_51_HOME,
				IRuntimeDetectionConstants.SEAM_20_HOME, IRuntimeDetectionConstants.SEAM_22_HOME,
				IRuntimeDetectionConstants.EAP_43_HOME};
		
		for (String path : paths) {
			assertTrue(path != null);
			File file = new File(path);
			assertTrue("The '" + path + "' path isn't valid.", file.isDirectory());
			RuntimePath runtimePath = new RuntimePath(path);
			runtimePaths.add(runtimePath);
		}
		RuntimeUIActivator.getDefault().saveRuntimePaths();
		runtimePaths = null;
		runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
		assertTrue("runtimePaths.size()\nExpected: 5\nWas: " + runtimePaths.size(), runtimePaths.size() == 5);
		List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(serverDefinitions);
			}
		}
		assertTrue("serverDefinitions.size()\nExpected: 0\nWas: " + serverDefinitions.size(), serverDefinitions.size() == 0);
	}
	
	@Test
	public void createRuntimes() {
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		List<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
		for (RuntimePath runtimePath : runtimePaths) {
			List<ServerDefinition> serverDefinitions = locator
					.searchForRuntimes(runtimePath.getPath(), new NullProgressMonitor());
			runtimePath.getServerDefinitions().clear();
			for (ServerDefinition serverDefinition : serverDefinitions) {
				serverDefinition.setRuntimePath(runtimePath);
			}
			runtimePath.getServerDefinitions()
					.addAll(serverDefinitions);
		}
		List<ServerDefinition> serverDefinitions = RuntimeUIActivator.getDefault().getServerDefinitions();
		assertTrue("serverDefinitions.size()\nExpected: >0\nWas: " + serverDefinitions.size(), serverDefinitions.size() > 0);
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				detector.initializeRuntimes(serverDefinitions);
			}
		}
	}
	
	@Test
	public void testSeamRuntimes() {
		SeamRuntime[] seamRuntimes = SeamRuntimeManager.getInstance().getRuntimes();
		assertTrue("seamRuntimes.length\nExpected: 4\nWas: " + seamRuntimes.length, seamRuntimes.length == 4);
		boolean seam20Exists = false;
		boolean seam22Exists = false;
		boolean seam12Exists = false;
		for(SeamRuntime seamRuntime:seamRuntimes) {
			SeamVersion version = seamRuntime.getVersion();
			if (SeamVersion.SEAM_2_0.equals(version)) {
				seam20Exists = true;
			}
			if (SeamVersion.SEAM_2_2.equals(version)) {
				seam22Exists = true;
			}
			if (SeamVersion.SEAM_1_2.equals(version)) {
				seam12Exists = true;
			}
		}
		assertTrue("Seam 2.0 Runtime isn't created.", seam20Exists);
		assertTrue("Seam 2.2 Runtime isn't created.", seam22Exists);
		assertTrue("Seam 1.2 Runtime isn't created.", seam12Exists);
	}
	
	@Test
	public void testWtpRuntimes() {
		IRuntime[] runtimes = ServerCore.getRuntimes();
		assertTrue("runtimes.length\nExpected: 3\nWas: " + runtimes.length, runtimes.length == 3);
		boolean as42Exists = false;
		boolean as51Exists = false;
		boolean eap43Exists = false;
		for(IRuntime runtime:runtimes) {
			IRuntimeType runtimeType = runtime.getRuntimeType();
			if (IJBossToolingConstants.AS_42.equals(runtimeType.getId())) {
				as42Exists = true;
			}
			if (IJBossToolingConstants.AS_51.equals(runtimeType.getId())) {
				as51Exists = true;
			}
			if (IJBossToolingConstants.EAP_43.equals(runtimeType.getId())) {
				eap43Exists = true;
			}
		}
		assertTrue("JBoss AS 4.2 Runtime isn't created.", as42Exists);
		assertTrue("JBoss AS 5.1 Runtime isn't created.", as51Exists);
		assertTrue("JBoss EAP 4.3 Runtime isn't created.", eap43Exists);
	}
}
