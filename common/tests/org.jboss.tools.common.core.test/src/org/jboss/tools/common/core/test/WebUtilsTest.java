/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.core.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.web.WebUtils;
import org.jboss.tools.test.util.ResourcesUtils;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class WebUtilsTest {

	protected IProject dynamicWebProject;
	protected IProject simpleDynamicWebProject;
	protected IProject resourceProject;
	protected IProject cordovaProject;
	protected IResource webContentDerived;
	protected IResource webContentDefault;
	protected IResource webContent;
	protected IResource simpleWebContent;
	protected IResource wwwWebContent;

	@Before
	public void setUp() throws Exception {
		if(dynamicWebProject==null) {
			dynamicWebProject = importProject("DynamicWebProject");
			webContentDerived = dynamicWebProject.findMember("WebContentDerived");
			assertNotNull(webContentDerived);
			webContentDerived.setDerived(true, null);
			webContentDefault = dynamicWebProject.findMember("WebContentDefault");
			assertNotNull(webContentDefault);
			webContent = dynamicWebProject.findMember("WebContent");
			assertNotNull(webContent);

			simpleDynamicWebProject = importProject("SimpleDynamicWebProject");
			simpleWebContent = simpleDynamicWebProject.findMember("WebContent");
			assertNotNull(simpleWebContent);

			resourceProject = importProject("ProjectResource");

			cordovaProject = importProject("CordovaProject");
			wwwWebContent = cordovaProject.findMember("www");
			assertNotNull(wwwWebContent);
		}
	}

	private static IProject importProject(String name) throws Exception {
		IProject project = ResourcesUtils.importProject("org.jboss.tools.common.core.test", "projects/" + name);
		assertNotNull(project);
		assertTrue(project.exists());
		return project;
	}

	@Test
	public void testGetWebRootFolder() {
		IFile file = dynamicWebProject.getFile("index4.html");
		IContainer root = WebUtils.getWebRootFolder(file);
		assertNull(root);

		file = dynamicWebProject.getFile("WebContent/index2.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(webContent, root);

		file = dynamicWebProject.getFile("WebContentDefault/index.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(webContentDefault, root);

		file = dynamicWebProject.getFile("WebContentDerived/index3.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(webContentDerived, root);

		file = dynamicWebProject.getFile("WebContent/doesNotExist.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(webContent, root);

		file = simpleDynamicWebProject.getFile("WebContent/index.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(simpleWebContent, root);

		file = resourceProject.getFile("WebContent/index.html");
		root = WebUtils.getWebRootFolder(file);
		assertNull(root);

		file = cordovaProject.getFile("www/index.html");
		root = WebUtils.getWebRootFolder(file);
		assertNotNull(root);
		assertEquals(wwwWebContent, root);
	}

	@Test
	public void testGeWebRootFolders() {
		IContainer[] roots = WebUtils.getWebRootFolders(dynamicWebProject, false);
		assertEquals(3, roots.length);
		assertEquals(webContentDefault, roots[0]);
		assertArrayContainsMemeber(roots, webContentDerived);
		assertArrayContainsMemeber(roots, webContent);

		roots = WebUtils.getWebRootFolders(dynamicWebProject, true);
		assertEquals(2, roots.length);
		assertEquals(webContentDefault, roots[0]);
		assertEquals(webContent, roots[1]);

		roots = WebUtils.getWebRootFolders(simpleDynamicWebProject, true);
		assertEquals(1, roots.length);
		assertEquals(simpleWebContent, roots[0]);

		roots = WebUtils.getWebRootFolders(resourceProject, false);
		assertEquals(0, roots.length);

		roots = WebUtils.getWebRootFolders(cordovaProject, true);
		assertEquals(1, roots.length);
		assertEquals(wwwWebContent, roots[0]);
	}

	@Test
	public void testGetWebContentPaths() {
		IPath[] paths = WebUtils.getWebContentPaths(dynamicWebProject);
		assertEquals(2, paths.length);
		assertEquals(webContentDefault.getFullPath(), paths[0]);
		assertEquals(webContent.getFullPath(), paths[1]);

		paths = WebUtils.getWebContentPaths(simpleDynamicWebProject);
		assertEquals(1, paths.length);
		assertEquals(simpleWebContent.getFullPath(), paths[0]);

		paths = WebUtils.getWebContentPaths(resourceProject);
		assertEquals(0, paths.length);

		paths = WebUtils.getWebContentPaths(cordovaProject);
		assertEquals(1, paths.length);
		assertEquals(wwwWebContent.getFullPath(), paths[0]);
	}

	@Test
	public void testFirstWebContentPath() {
		IPath path = WebUtils.getFirstWebContentPath(dynamicWebProject);
		assertEquals(webContentDefault.getFullPath(), path);

		path = WebUtils.getFirstWebContentPath(simpleDynamicWebProject);
		assertEquals(simpleWebContent.getFullPath(), path);

		path = WebUtils.getFirstWebContentPath(resourceProject);
		assertNull(path);

		path = WebUtils.getFirstWebContentPath(cordovaProject);
		assertEquals(wwwWebContent.getFullPath(), path);
	}

	@Test
	public void testFindResource() {
		
		// Dynamic Web Project
		IFile context1 = dynamicWebProject.getFile("WebContent/pages/index.html");
		IFile context2 = dynamicWebProject.getFile("WebContentDefault/pages/indexF1.html");
		assertTrue("Context file not found", context1.exists() && context1.isAccessible());
		assertTrue("Context file not found", context2.exists() && context2.isAccessible());

		File testFile = dynamicWebProject.getFile("WebContent/css/some.css").getLocation().toFile(); 
		assertTrue("Context file not found", testFile.exists() && testFile.canRead());
				
		// Relative path
		IResource resource = WebUtils.findResource(context1, "../css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		resource = WebUtils.findResource(context2, "../css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute path in project
		resource = WebUtils.findResource(context1, "/css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		resource = WebUtils.findResource(context2, "/css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute location path
		IPath location = dynamicWebProject.getLocation()
				.append("WebContent")
				.append(new Path("css/some.css"));
		resource = WebUtils.findResource(context1, location.toString());
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		resource = WebUtils.findResource(context2, location.toString());
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		
		// Simple Web Project
		IFile context = simpleDynamicWebProject.getFile("WebContent/pages/index.html");
		assertTrue("Context file not found", context.exists() && context.isAccessible());

		testFile = simpleDynamicWebProject.getFile("WebContent/css/some.css").getLocation().toFile(); 
		assertTrue("Context file not found", testFile.exists() && testFile.canRead());

		// Relative path
		resource = WebUtils.findResource(context, "../css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute path in project
		resource = WebUtils.findResource(context, "/css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute location path
		location = simpleDynamicWebProject.getLocation()
				.append("WebContent")
				.append(new Path("css/some.css"));
		resource = WebUtils.findResource(context, location.toString());
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		
		// Resource Web Project
		context = resourceProject.getFile("WebContent/pages/index.html");
		assertTrue("Context file not found", context.exists() && context.isAccessible());
		
		testFile = resourceProject.getFile("WebContent/css/some.css").getLocation().toFile(); 
		assertTrue("Context file not found", testFile.exists() && testFile.canRead());
		
		// Relative path
		resource = WebUtils.findResource(context, "../css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute path in project
		resource = WebUtils.findResource(context, "/css/some.css");
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
		// Absolute location path
		location = resourceProject.getLocation()
				.append("WebContent")
				.append(new Path("css/some.css"));
		resource = WebUtils.findResource(context, location.toString());
		assertTrue(resource != null && resource.getLocation().toFile().equals(testFile));
	}

	@Test
	public void testGetWebPath() {
		IFile context = cordovaProject.getFile("www/index.html");
		IFile resource = cordovaProject.getFile("www/img/pic.gif");
		String path = WebUtils.getWebPath(context, resource);
		assertEquals("img/pic.gif", path);
		path = WebUtils.getWebPath(null, resource);
		assertEquals("/img/pic.gif", path);

		context = dynamicWebProject.getFile("WebContent/index2.html");
		resource = dynamicWebProject.getFile("WebContent/css/some.css");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("css/some.css", path);
		context = dynamicWebProject.getFile("WebContent/pages/index.html");
		resource = dynamicWebProject.getFile("WebContent/css/some.css");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("../css/some.css", path);
		context = dynamicWebProject.getFile("WebContent/index2.html");
		resource = dynamicWebProject.getFile("WebContent/pages/index.html");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("pages/index.html", path);
		context = dynamicWebProject.getFile("WebContent/pages/index.html");
		resource = dynamicWebProject.getFile("WebContent/index2.html");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("../index2.html", path);
		path = WebUtils.getWebPath(null, resource);
		assertEquals("/index2.html", path);
		context = dynamicWebProject.getFile("WebContentDefault/pages/indexF1.html");
		resource = dynamicWebProject.getFile("WebContent/pages/index.html");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("../../WebContent/pages/index.html", path);

		context = dynamicWebProject.getFile("WebContentDefault/pages/indexF1.html");
		resource = resourceProject.getFile("WebContent/index.html");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("/WebContent/index.html", path);

		context = resourceProject.getFile("WebContent/index.html");
		resource = dynamicWebProject.getFile("WebContentDefault/pages/indexF1.html");
		path = WebUtils.getWebPath(context, resource);
		assertEquals("/pages/indexF1.html", path);
	}

	private void assertArrayContainsMemeber(Object[] array, Object member) {
		StringBuilder sb = new StringBuilder("[");
		for (int i = 0; i < array.length; i++) {
			sb.append("\"").append(sb.toString()).append("\", ");
			if(array[i].equals(member)) {
				return;
			}
		}
		sb.append("]");
		fail("Array " + sb.toString() + " doesn't contain \"" + member + "\"");
	}
}