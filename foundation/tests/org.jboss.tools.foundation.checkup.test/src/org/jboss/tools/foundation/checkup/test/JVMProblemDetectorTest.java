/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.checkup.test;

import java.io.InputStream;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.widgets.Display;
import org.jboss.tools.foundation.checkup.JVMProblemDetector;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.Dependant;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.DependantList;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.UnresolvedModule;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.UnresolvedStructure;
import org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages;
import org.jboss.tools.foundation.checkup.JVMProblemDialog;
import org.jboss.tools.foundation.checkup.JVMProblemDialog.LabelProvider;
import org.jboss.tools.foundation.checkup.JVMProblemDialog.TreeContent;
import org.junit.Assert;
import org.junit.Test;


public class JVMProblemDetectorTest{
	public static final String UNRESOLVED_MODULE = "org.jboss.tools.foundation.checkup.testplugin requires JavaSE version 1.9";  //$NON-NLS-1$
	
	@Test
	public void testJVMProblemDetector(){
		List<UnresolvedModule> modules = getUnresolvedModules();
		
		Assert.assertEquals("Wrong number of unresolved modules", 1, modules.size());
		
//		for(UnresolvedModule module : modules){
//			System.out.println("Unresolved module - "+module.toString());
//		}
		
		Assert.assertEquals("Wrong unresolved module", UNRESOLVED_MODULE, modules.get(0).toString());
		
	}
	
	@Test
	public void testJVMProblemDialogProviders(){
		List<UnresolvedModule> modules = getUnresolvedModules();
		
		JVMProblemDialog dialog = new JVMProblemDialog(Display.getDefault().getActiveShell(), modules, "JavaSE-1.8");
		
		TreeContent contentProvider = dialog.new TreeContent();
		LabelProvider labelProvider = dialog.new LabelProvider();
		
		
		Object[] children = contentProvider.getChildren(modules);
		
		// UnresolvedModule
		Assert.assertEquals("Unexpected number of unresolved modules", 1, children.length);
		
		Assert.assertTrue("Dialog Content Provider should return instanceof UnresolvedModule", children[0] instanceof UnresolvedModule);
		
		UnresolvedModule module = (UnresolvedModule) children[0];
		
		Assert.assertEquals("Unexpected label for UnresolvedModule", UNRESOLVED_MODULE, labelProvider.getText(module));
		
		children = contentProvider.getChildren(module);
		
		// DependantList
		Assert.assertEquals("Unexpected number of unresolved modules children", 1, children.length);
		
		Assert.assertTrue("Dialog Content Provider should return instanceof UnresolvedModule", children[0] instanceof DependantList);
		
		DependantList list = (DependantList) children[0];
		
		Assert.assertEquals("Unexpected label for DependantList", JVMProblemDetectorMessages.DEPENDANT_MODULES, labelProvider.getText(list));
		
		children = contentProvider.getChildren(list);
		
		// Dependant
		Assert.assertEquals("Unexpected number of dependant modules", 1, children.length);
		
		Assert.assertTrue("Dialog Content Provider should return instanceof Dependant", children[0] instanceof Dependant);
		
		Dependant dependant = (Dependant) children[0];
		
		Assert.assertEquals("Unexpected label for Dependant", "org.jboss.tools.foundation.checkup.test", labelProvider.getText(dependant));
		
	}
	
	@Test
	public void testOneSessionDateBefore() throws ParseException, URISyntaxException{
		JVMProblemDetector detector = new JVMProblemDetector();
		
		detector.setEclipseStartTime("2015-07-07 14:38:03.531");
		
		InputStream stream = JVMProblemDetectorTest.class.getClassLoader().getResourceAsStream("/resources/log1.txt");
		
		Assert.assertNotNull(stream);
		
		detector.readLogFile(stream);
		
		Assert.assertTrue(detector.getUnresolvedStructure().getUnresolvedModules().size()>0);
	}

	@Test
	public void testOneSessionDateAfter() throws ParseException{
		JVMProblemDetector detector = new JVMProblemDetector();
		
		detector.setEclipseStartTime("2015-07-07 15:38:03.531");
		
		InputStream stream = JVMProblemDetectorTest.class.getClassLoader().getResourceAsStream("/resources/log1.txt");
		
		Assert.assertNotNull(stream);
		
		detector.readLogFile(stream);
		
		Assert.assertTrue(detector.getUnresolvedStructure().getUnresolvedModules().size()==0);
	}

	@Test
	public void testTwoSessionsDateBefore() throws ParseException{
		JVMProblemDetector detector = new JVMProblemDetector();
		
		detector.setEclipseStartTime("2015-07-07 14:38:03.531");
		
		InputStream stream = JVMProblemDetectorTest.class.getClassLoader().getResourceAsStream("/resources/log2.txt");
		
		Assert.assertNotNull(stream);
		
		detector.readLogFile(stream);
		
		Assert.assertTrue(detector.getUnresolvedStructure().getUnresolvedModules().size()==0);
	}

	@Test
	public void testTwoSessionsDateAfter() throws ParseException{
		JVMProblemDetector detector = new JVMProblemDetector();
		
		detector.setEclipseStartTime("2015-07-07 15:38:03.531");
		
		InputStream stream = JVMProblemDetectorTest.class.getClassLoader().getResourceAsStream("/resources/log2.txt");
		
		Assert.assertNotNull(stream);
		
		detector.readLogFile(stream);
		
		Assert.assertTrue(detector.getUnresolvedStructure().getUnresolvedModules().size()==0);
		
	}
	
//	private List<UnresolvedModule> getUnresolvedModules(){
//		UnresolvedStructure uresolvedStructure = JVMProblemDetector.getUnresolvedStructure();
//		assertNotNull(uresolvedStructure);
//		
//		List<UnresolvedModule> modules = uresolvedStructure.getUnresolvedModules();
//		assertNotNull(modules);		
//		return modules;
//	}

	private List<UnresolvedModule> getUnresolvedModules(){
		UnresolvedStructure uresolvedStructure = JVMProblemDetector.getUnresolvedStructure();
		
		ArrayList<String> list = new ArrayList<String>();
		list.add("org.jboss.tools.foundation.checkup.test");
		
		uresolvedStructure.addRequieredJava("org.jboss.tools.foundation.checkup.testplugin", list, "JavaSE", "1.9");
		
		List<UnresolvedModule> modules = uresolvedStructure.getUnresolvedModules();
		Assert.assertNotNull(modules);		
		return modules;
	}
}
