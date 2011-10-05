package org.jboss.tools.common.jdt.core.buildpath;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.common.jdt.core.JDTExtActivator;
import org.jboss.tools.common.jdt.core.Messages;

public class MaterializeLibraryJob extends WorkspaceJob {

	private final IFolder libFolder;
	private final IJavaProject javaProject;
	private final Map<IPath, String> jars;
	private final IClasspathContainer containerToRemove;

	public MaterializeLibraryJob(IJavaProject javaProject,
								 IClasspathContainer containerToMaterialize,
								 Map<IPath, String> jars, 
								 IFolder libFolder) {
		super(Messages.Materialize_Library); 
		if (javaProject == null || javaProject.getProject() == null) {
			throw new IllegalArgumentException("Project must not be null");
		}
		if (containerToMaterialize == null) {
			throw new IllegalArgumentException("Container to materialize must not be null");
		}
		if (libFolder == null) {
			throw new IllegalArgumentException("Destination library folder must not be null");
		}
		this.javaProject = javaProject;
		this.libFolder = libFolder;
		this.containerToRemove = containerToMaterialize;
		this.jars = jars;
	}

	@Override
	public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {

		final IPath rootPath = ResourcesPlugin.getWorkspace().getRoot().getLocation();
		
		IPath containerToRemovePath = containerToRemove.getPath();
		
		LinkedHashSet<IClasspathEntry> newCpes = null;
		
		//Create new Classpath entries
		if (jars != null && !jars.isEmpty()) {
			newCpes = copyClasspathEntries(monitor,	rootPath);	
		}
		
		// Remove the Classpath Library first
		removeClasspathContainer(javaProject, containerToRemovePath);

		// Then add the new Classpath entries
		if (newCpes != null && !newCpes.isEmpty()) {
			IClasspathEntry[] entries = new IClasspathEntry[newCpes.size()];
			newCpes.toArray(entries);
			addToClasspath(javaProject, entries, monitor);
		}

		//Finally execute post processors (needed to remove project nature, for ex.)
		for (ILibraryMaterializationPostProcessor processor : getPostProcessors()) {
			processor.execute(javaProject, containerToRemovePath);
		}

		javaProject.getProject().refreshLocal(IResource.DEPTH_INFINITE, monitor);

		return Status.OK_STATUS;
	}

	private LinkedHashSet<IClasspathEntry> copyClasspathEntries(
			IProgressMonitor monitor, final IPath rootPath)
			throws CoreException, JavaModelException {
		
		int jarSize = jars.size();
		
		monitor.beginTask(Messages.Materialize_Library, jarSize); 
		
		mkdirs(libFolder, monitor);

		IPath destination = libFolder.getLocation();

		LinkedHashSet<IClasspathEntry> newCpes = new LinkedHashSet<IClasspathEntry>(jarSize);

		IClasspathEntry[] cpEntries = containerToRemove.getClasspathEntries();

		for (IClasspathEntry entry : cpEntries) {
			if (entry.getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
				
				IPath sourceFilePath = entry.getPath();
				String fileName = jars.get(sourceFilePath);
				if (fileName == null) {
					// Jar was not selected
					continue;
				}
				
				monitor.subTask(fileName);
				IPath destinationFilePath = destination.append(fileName);
				try {
					if ((javaProject.findPackageFragmentRoot(destinationFilePath) == null)// Not already defined
							&& copy(sourceFilePath, destinationFilePath)) {
						// TODO handle duplicate files -> rename?
						// FIXME use absolute / relative paths depending on the
						// location of the destination folder (in project /
						// elsewhere in the workspace)
						// FIXME need a more elegant way to get a relative path
						// with a leading /
						IPath relativePath = new Path("/").append(libFolder.getFullPath()).append(fileName); //$NON-NLS-1$
						IClasspathEntry newEntry = getNewClasspathEntry(entry, relativePath);
						newCpes.add(newEntry);
					}
				} catch (IOException e) {
					IStatus status = new Status(IStatus.ERROR,
							JDTExtActivator.PLUGIN_ID, NLS.bind(Messages.MaterializeLibraryJob_error_copying_file, fileName), e);
					throw new CoreException(status);
				}
			} else if (entry.getEntryKind() == IClasspathEntry.CPE_PROJECT) {
				newCpes.add(entry);
			}
		}
		return newCpes;
	}

	private ILibraryMaterializationPostProcessor[] getPostProcessors() {
		ILibraryMaterializationPostProcessorFactory ppFactory = JDTExtActivator.getDefault()
																  .getLibraryMaterializationPostProcessorFactory();
		ILibraryMaterializationPostProcessor[] postProcessors = ppFactory.getLibraryMaterializationPostProcessors();
		return postProcessors;
	}

	private IClasspathEntry getNewClasspathEntry(IClasspathEntry entry,
			IPath destinationFilePath) throws CoreException {
		try {
			return JavaCore.newLibraryEntry(destinationFilePath,
					entry.getSourceAttachmentPath(),
					entry.getSourceAttachmentRootPath(),
					entry.getAccessRules(), entry.getExtraAttributes(),
					entry.isExported());
		} catch (Exception e) {
			IStatus status = new Status(IStatus.ERROR, JDTExtActivator.PLUGIN_ID, 
					NLS.bind(Messages.MaterializeLibraryJob_Error_creating_classpath_entry, e.getMessage()), e);
			throw new CoreException(status);
		}
	}

	private void addToClasspath(IJavaProject javaProject,
			IClasspathEntry[] newCpes, IProgressMonitor monitor)
			throws JavaModelException {
		if (newCpes.length > 0) {
			IClasspathEntry[] originalClasspathEntries = javaProject.getRawClasspath();
			IClasspathEntry[] newClasspath = new IClasspathEntry[originalClasspathEntries.length + newCpes.length];
			System.arraycopy(originalClasspathEntries, 0, newClasspath, 0,	originalClasspathEntries.length);
			System.arraycopy(newCpes, 0, newClasspath, originalClasspathEntries.length, newCpes.length);
			javaProject.setRawClasspath(newClasspath, monitor);
		}
	}

	private boolean copy(IPath sourceFilePath, IPath destinationFilePath)
			throws IOException {
		File sourcefile = sourceFilePath.toFile();
		if (sourcefile.isFile()) {
			File destinationFile = destinationFilePath.toFile();
			//Overwrite existing file
			FileUtils.copyFile(sourcefile, destinationFile);
			if (destinationFile.exists()) {
				return true;
			}
		}
		return false;
	}

	private void removeClasspathContainer(IJavaProject javaProject,
			IPath containerToRemovePath) throws JavaModelException {
		if (javaProject != null) {
			// remove classpatch container from JavaProject
			ArrayList<IClasspathEntry> newEntries = new ArrayList<IClasspathEntry>();
			for (IClasspathEntry entry : javaProject.getRawClasspath()) {
				if (!containerToRemovePath.equals(entry.getPath())) {
					newEntries.add(entry);
				}
			}
			javaProject.setRawClasspath(newEntries.toArray(new IClasspathEntry[newEntries.size()]),	null);
		}
	}

	private void mkdirs(final IFolder folder, IProgressMonitor monitor)
			throws CoreException {
		if (!folder.exists()) {
			if (folder.getParent() instanceof IFolder) {
				mkdirs((IFolder) folder.getParent(), monitor);
			}
			folder.create(true /* force */, true /* local */, monitor);
		} else {
			IContainer x = folder;
			while (x instanceof IFolder && x.isDerived()) {
				x.setDerived(false, monitor);
				x = x.getParent();
			}
		}

	}
}