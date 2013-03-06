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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import junit.framework.TestCase;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.handlers.TestHandler1;

public class RuntimeDetectionTest extends TestCase {

	protected void tearDown() throws Exception {
		IPath path = RuntimeTestActivator.getDefault().getStateLocation();
		File root = path.toFile();
		File[] children = root.listFiles();
		for( int i = 0; i < children.length; i++ ) {
			deleteAll(children[i]);
		}
	}
	
	private void deleteAll(File f) {
		if( !f.isFile()) {
			File[] children = f.listFiles();
			for( int i = 0; i < children.length; i++ ) {
				deleteAll(children[i]);
			}
		}
		f.delete();
	}
	
	public void testHandlerEnabled() {
		RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		TestHandler1.getInstance().reset();
		runDiscovery(true);
	}
	public void testHandlerDisabled() {
		RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		TestHandler1.getInstance().reset();
		setHandlerEnablement(false);
		try {
			runDiscovery(false);
		} finally {
			setHandlerEnablement(true);
		}
	}
	
	private void setHandlerEnablement(boolean enabled) {
		IRuntimeDetector det = RuntimeCoreActivator.getDefault().findRuntimeDetector("org.jboss.tools.runtime.handlers.TestHandler1");
		det.setEnabled(enabled);
	}
	
	private void runDiscovery(boolean enabled) {
		IPath path = RuntimeTestActivator.getDefault().getStateLocation();
		RuntimePath rp = new RuntimePath(path.toFile().getAbsolutePath());
		assertEquals(0, rp.getRuntimeDefinitions().length);
		RuntimeInitializerUtil.createRuntimeDefinitions(
				new RuntimePath[]{rp}, new NullProgressMonitor());
		if( !enabled ) 
			assertEquals(0, rp.getRuntimeDefinitions().length);
		else {
			assertEquals(1, rp.getRuntimeDefinitions().length);
			RuntimeDefinition def1 = rp.getRuntimeDefinitions()[0];
			assertNotNull(def1);
			assertEquals(def1.getType(), "testHandler");
		}
	}
	
	public void testDiscoveryAndInitialize() {
		RuntimeCoreActivator.getDefault().getRuntimeDetectors();
		TestHandler1.getInstance().reset();
		IPath root = RuntimeTestActivator.getDefault().getStateLocation();
		IPath rootA = root.append("folderA");
		IPath rootB = root.append("folderB");
		rootA.toFile().mkdirs();
		rootB.toFile().mkdirs();
		
		RuntimePath rpA = new RuntimePath(rootA.toFile().getAbsolutePath());
		RuntimePath rpB = new RuntimePath(rootB.toFile().getAbsolutePath());
	
		assertEquals(0, rpA.getRuntimeDefinitions().length);
		assertEquals(0, rpB.getRuntimeDefinitions().length);
		
		RuntimePath[] rpArray = new RuntimePath[]{rpA, rpB};
		
		RuntimeInitializerUtil.createRuntimeDefinitions(
				rpArray, new NullProgressMonitor());

		assertEquals(1, rpA.getRuntimeDefinitions().length);
		assertEquals(1, rpB.getRuntimeDefinitions().length);
		
		ArrayList<RuntimeDefinition> defs = new ArrayList<RuntimeDefinition>();
		defs.addAll(Arrays.asList(rpA.getRuntimeDefinitions()));
		defs.addAll(Arrays.asList(rpB.getRuntimeDefinitions()));
		
		IRuntimeDetector det = RuntimeCoreActivator.getDefault().findRuntimeDetector("org.jboss.tools.runtime.handlers.TestHandler1");
		det.initializeRuntimes(defs);
		
		String[] inited = TestHandler1.getInstance().getInited();
		assertEquals(2, inited.length);
		TestHandler1.getInstance().reset();
	}
	
}
