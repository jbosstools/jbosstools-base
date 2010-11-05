package org.jboss.tools.common.el.core.test.resolver;

import java.io.IOException;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactoryManager;
import org.jboss.tools.test.resource.ResourceFactory;
import org.junit.Test;

public class ELResolverFactoryManagerTest extends TestCase{

	public void testGetInstance() {
		assertNotNull(ELResolverFactoryManager.getInstance());
	}

	public void testGetResolvers() throws CoreException, IOException {
		assertNotNull(ELResolverFactoryManager.getInstance().getResolvers(ResourceFactory.createFile("test")));
		assertTrue(ELResolverFactoryManager.getInstance().getResolvers(ResourceFactory.createFile("test")).length==0);
		
		String fileContent = "<html><body var=\"test1\" value=\"#{value1}\"><p var=\"test2\" value=\"#{value2}\"/></body></html>";
		IFile file = ResourceFactory.createFile(fileContent,"Test1","test1.xml");
		IProjectDescription descr = file.getProject().getDescription();
		descr.setNatureIds(new String[] {ProjectNature1.ID});
		file.getProject().setDescription(descr, null);
		ELResolver[] resolvers = ELResolverFactoryManager.getInstance().getResolvers(file);
		assertTrue("Expected 2 relovers to be loaded because of other two don't implement appropriate interfaces",resolvers.length==2);
	}

}
