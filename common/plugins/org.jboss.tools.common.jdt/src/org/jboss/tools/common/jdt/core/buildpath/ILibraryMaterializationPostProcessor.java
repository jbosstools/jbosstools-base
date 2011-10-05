package org.jboss.tools.common.jdt.core.buildpath;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IJavaProject;

public interface ILibraryMaterializationPostProcessor {

	void execute(IJavaProject javaProject, IPath containerPath) throws CoreException;

 	boolean applies(IJavaProject javaProject, IPath containerPath) throws CoreException;

}
