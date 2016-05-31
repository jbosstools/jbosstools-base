package org.jboss.tools.common.text.ext.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class StructuredModelWrapperTest {

	@BeforeClass
	public static void createProject() throws InvocationTargetException, IOException, CoreException, InterruptedException {
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject("smtest");
		project.create(null);
		project.open(null);
		IFile xmlFile = project.getFile("test1.xml");
		xmlFile.create(
				new ByteArrayInputStream(
						("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
						"<root/>").getBytes()), 
						IResource.FORCE, null);
		IFile htmlFile = project.getFile("test1.html");
		htmlFile.create(
				new ByteArrayInputStream(
						("<!DOCTYPE html>\n" +
								"<html ng-app=\"test\">\n" +
								"<body>\n" +
								"</body>\n" +
								"</html>").getBytes()), 
						IResource.FORCE, null);
	}

	@Test
	public void testExecuteForHtmlDocument() {
		IFile testFile = ResourcesPlugin.getWorkspace().getRoot().getProject("smtest").getFile("test1.html");
		StructuredModelWrapper.execute(testFile, xmlDocument -> { assertNotNull(xmlDocument); });
	}

	@Test
	public void testExecuteForXmlDocument() {
		IFile testFile = ResourcesPlugin.getWorkspace().getRoot().getProject("smtest").getFile("test1.xml");
		StructuredModelWrapper.execute(testFile, xmlDocument -> { assertNotNull(xmlDocument); });
	}

	@Test
	public void testExecuteForXmlDocumentICommand2() {
		IFile testFile = ResourcesPlugin.getWorkspace().getRoot().getProject("smtest").getFile("test1.xml");
		String result = StructuredModelWrapper.execute(testFile, model -> {
				assertNotNull(model);
				return model.getStructuredDocument()
						.getFirstStructuredDocumentRegion()
						.getNext() // <?xml ...>
						.getNext() // '\n'
						.getText();// <root/> 
				});
		assertNotNull(result);
		assertEquals("<root/>",result);
	}

	@AfterClass
	public static void removeProject() {
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject("smtest");
		try {
			project.delete(IResource.FORCE, null);
		} catch (CoreException e) {
			IStatus status = new Status(IStatus.ERROR,"org.jboss.tools.common.text.ext.test","Could not remove test project");
			Platform.getLog(Platform.getBundle("org.jboss.tools.common.text.ext.test")).log(status);
		}
	}
}
