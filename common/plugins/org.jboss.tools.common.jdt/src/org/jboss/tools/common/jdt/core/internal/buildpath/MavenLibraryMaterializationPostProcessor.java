package org.jboss.tools.common.jdt.core.internal.buildpath;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IJavaProject;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.jdt.core.buildpath.ILibraryMaterializationPostProcessor;

public class MavenLibraryMaterializationPostProcessor implements ILibraryMaterializationPostProcessor {

  private static final String MAVEN_NATURE_ID = "org.eclipse.m2e.core.maven2Nature";
  
  public boolean applies(IJavaProject javaProject, IPath containerPath) throws CoreException {
	  boolean applies = javaProject != null && javaProject.getProject().hasNature(MAVEN_NATURE_ID)
	  && ("org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER".equals(containerPath.toPortableString()));	  
	  return applies;
  }
  
  public void execute(IJavaProject javaProject, IPath containerPath) throws CoreException {
    if (applies(javaProject, containerPath)) {
      System.err.println("Removing "+ MAVEN_NATURE_ID + " from "+ javaProject.getProject().getName());
      EclipseUtil.removeNatureFromProject(javaProject.getProject(), MAVEN_NATURE_ID);
    }
  }

}
