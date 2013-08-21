/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.test;

import java.io.IOException;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

/**
 *   
 * @author V.Kabanovich
 *
 */
public class PropertiesLoaderTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider = null;
	IProject project = null;

	public PropertiesLoaderTest() {}

	public void setUp() throws Exception {
		provider = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project = provider.getProject();

		project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testMalformedPropertiesFile() throws CoreException, IOException {
		IFile f = project.getFile(new Path("src/x.properties"));
		assertNotNull(f);
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		XModelObject a = p.getChildByPath("a");
		assertNotNull(a);
		assertEquals(a.getAttributeValue("value"), "valueA");
		XModelObject b = p.getChildByPath("b");
		assertNull(b);
		XModelObject c = p.getChildByPath("c");
		assertNotNull(c);
		assertEquals(c.getAttributeValue("value"), "valueC");
	}

	public void testPropertiesThatEndWithMultipleBackslash() throws CoreException, IOException {
		IFile f = project.getFile(new Path("src/backslash.properties"));
		assertNotNull(f);
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);

		XModelObject p1 = p.getChildByPath("no_backslash");
		assertNotNull(p1);
		assertEquals(p1.getAttributeValue("value"), "one line");

		XModelObject p2 = p.getChildByPath("one_backslash");
		assertNotNull(p2);
		assertEquals(p2.getAttributeValue("value"), "first line second line");

		XModelObject p3 = p.getChildByPath("two_backslash");
		assertNotNull(p3);
		assertEquals(p3.getAttributeValue("value"), "also one line \\");

		XModelObject p4 = p.getChildByPath("three_backslash");
		assertNotNull(p4);
		assertEquals(p4.getAttributeValue("value"), "again first line \\again second line");
	}

	public void testLoadingPropertiesDoesNotModifyContent() throws CoreException {
		IFile f = project.getFile(new Path("src/y.properties"));
		String text = FileUtil.readStream(f);
		System.out.println(text);
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		String newText = ((FileAnyImpl)p).getAsText();
		System.out.println(newText);
		assertEquals(text, newText);

	}

	/**
	 * 1. Load file a1.properties. This file contains 'multiple' value,
	 * unsupported by java.util.Properties
	 * 2. Read property 'a'
	 * 3. Modify its value.
	 * 4. Compare the result to be saved to content of a1-res.properties.
	 * @throws CoreException
	 */
	public void testModificationOfFileWithMultipleProperty() throws CoreException {
		IFile f = project.getFile(new Path("src/a1.properties"));
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		p.getChildByPath("a").setAttributeValue("value", "bc2");
		String newText = ((FileAnyImpl)p).getAsText();
		IFile fres = project.getFile(new Path("src/a1-res.properties"));
		String resText = FileUtil.readStream(fres);
		assertEquals(newText, resText);
	}

	/**
	 * 1. Load file a2.properties;
	 * 2. Read property 'p9'
	 * 3. Modify its value - modification takes only one of many properties in the file.
	 * 4. Compare the result to be saved to content of a2-res.properties.
	 * @throws CoreException
	 */
	public void testModificationOfFileWithVariousFormattedProperties() throws CoreException {
		IFile f = project.getFile(new Path("src/a2.properties"));
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		XModelObject p9 = p.getChildByPath("p9");
		String value = p9.getAttributeValue("value");
		p9.setAttributeValue("value", value.substring(0, 4) + value.substring(5, value.length()));
		String newText = ((FileAnyImpl)p).getAsText();
		IFile fres = project.getFile(new Path("src/a2-res.properties"));
		String resText = FileUtil.readStream(fres);
		assertEquals(newText, resText);
	}

	/**
	 * 1. Load file a3.properties;
	 * 2. Read property 'p'
	 * 3. Modify its value - modification takes 2 lines.
	 * 4. Compare the result to be saved to content of a3-res.properties.
	 * @throws CoreException
	 */
	public void testModificationOfFormattedPropertyAffectingTwoLines() throws CoreException {
		IFile f = project.getFile(new Path("src/a3.properties"));
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		XModelObject p9 = p.getChildByPath("p");
		String value = p9.getAttributeValue("value");
		p9.setAttributeValue("value", value.substring(0, 5) + "3" + value.substring(8, value.length()));
		String newText = ((FileAnyImpl)p).getAsText();
		IFile fres = project.getFile(new Path("src/a3-res.properties"));
		String resText = FileUtil.readStream(fres);
		assertEquals(newText, resText);
	}

}
