/*************************************************************************************
 * Copyright (c) 2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.test;

import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.internal.InvalidRuntimeDetector;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.handlers.TestHandler1;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;

/**
 * 
 * @author rob stryker
 * 
 */
public class RuntimeDetectionFrameworkTest extends TestCase {

	@BeforeClass
	public static void create() {
		RuntimeCoreActivator.getDefault();
		RuntimeUIActivator.getDefault();
	}

	@Test
	public void testInvalidDetectors() {
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getDeclaredRuntimeDetectors();
		IRuntimeDetector invalidDetector = null;
		for (IRuntimeDetector detector:detectors) {
			if (detector instanceof InvalidRuntimeDetector) {
				invalidDetector = detector;
			}
		}
		assertFalse("Invalid detector is enabled.", invalidDetector.isEnabled());
	}
	
	private String displayRuntimes(RuntimePath[] paths) {
		String s = "";
		for(RuntimePath path:paths) {
			s += path.getPath() + "\n";
		}
		return s;
	}
	
	@Test
	public void testLoadSaveRuntimePaths() {
		String path = "test/path/one";
		RuntimePath[] runtimePaths = RuntimeUIActivator.getRuntimePaths();
		assertEquals(displayRuntimes(runtimePaths), 0, runtimePaths.length);
		RuntimePath runtimePath = new RuntimePath(path);
		runtimePath.setScanOnEveryStartup(false);
		RuntimeUIActivator.getDefault().getModel().addRuntimePath(runtimePath);
		RuntimeUIActivator.getDefault().getModel().saveRuntimePaths();
		restartBundle();
		runtimePaths = RuntimeUIActivator.getRuntimePaths();
		assertEquals(1, runtimePaths.length);
		RuntimeUIActivator.getDefault().getModel().setRuntimePaths(new RuntimePath[]{});
		restartBundle();
		runtimePaths = RuntimeUIActivator.getRuntimePaths();
		assertEquals(0, runtimePaths.length);
	}
	
	private void restartBundle() {
		Bundle bundle = Platform.getBundle(RuntimeUIActivator.PLUGIN_ID);
		try {
			// reload prefs
			bundle.stop();
			bundle.start();
		} catch(BundleException be) {
			
		}
	}
	
	@Before
	public void setUp() {
		IPath stateLoc = RuntimeTestActivator.getDefault().getStateLocation();
		stateLoc.append("a").toFile().mkdirs();
		stateLoc.append("b").toFile().mkdirs();
		stateLoc.append("c").toFile().mkdirs();
	}
	
	@After
	public void tearDown() {
		IPath stateLoc = RuntimeTestActivator.getDefault().getStateLocation();
		stateLoc.append("a").toFile().delete();
		stateLoc.append("b").toFile().delete();
		stateLoc.append("c").toFile().delete();
	}
	
	@Test
	public void testInitializationPaths() {
		IPath p = RuntimeTestActivator.getDefault().getStateLocation();
		String path = p.toFile().getAbsolutePath();
		
		// Create our path
		RuntimePath runtimePath = new RuntimePath(path);
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		
		// Use the locator to find our runtime defs.
		List<RuntimeDefinition> runtimeDefinitions = locator
				.searchForRuntimes(runtimePath.getPath(), new NullProgressMonitor());
		assertEquals(3, runtimeDefinitions.size());
		
		
		// initialize them
		Set<IRuntimeDetector> detectors = RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		for( IRuntimeDetector detector:detectors) {
			if (detector.isEnabled()) {
				System.out.println("initializing for detector " + detector.getId());
				detector.initializeRuntimes(runtimeDefinitions);
			}
		}
		TestHandler1 handler = TestHandler1.getInstance();
		String[] initialized = handler.getInited();
		if( initialized.length == 0 ) {
			// debug
			System.out.println("HERE inspecting handler " + handler.toString()); 
		}
		assertEquals(3, initialized.length);
		
	}
}
