/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.test.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.StringBufferInputStream;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.eclipse.ui.wizards.datatransfer.ZipFileStructureProvider;
import org.jboss.tools.tests.ImportProvider;
import org.osgi.framework.Bundle;

/**
 * @author eskimo
 * 
 */
public class ResourcesUtils {
	
	public static IProject importProject( 
			Bundle bundle, String templLocation, 
			IProgressMonitor monitor) 
		throws 
			IOException, CoreException, 
			InvocationTargetException, InterruptedException  {
		
		return importProject(bundle, templLocation, new Path(templLocation).lastSegment(), monitor);
	}

	public static IProject importProject( 
			Bundle bundle, String templLocation, String projectName, 
			IProgressMonitor monitor) 
		throws 
			IOException, CoreException, 
			InvocationTargetException, InterruptedException  {
		
		String tplPrjLcStr;
			tplPrjLcStr = FileLocator.resolve(bundle.getEntry(templLocation))
				.getFile();
			String protocol = FileLocator.resolve(bundle.getEntry(templLocation)).getProtocol();
			IProject importedPrj = importProjectIntoWorkspace(tplPrjLcStr, projectName, protocol);
		return importedPrj;
	}
	
	public static IProject importProject( 
			Bundle bundle, String templLocation) throws IOException, CoreException, InvocationTargetException, InterruptedException {
		return importProject(bundle, templLocation, new NullProgressMonitor());
	}
	
	public static IProject createEclipseProject(String projectName,
			IProgressMonitor monitor) throws CoreException {

		IProject newProjectHandle = ResourcesPlugin.getWorkspace().getRoot()
				.getProject(projectName);

		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IProjectDescription description = workspace
				.newProjectDescription(projectName);
		newProjectHandle.create(description, new NullProgressMonitor());
		newProjectHandle.open(monitor);
		return newProjectHandle;
	}

	public static IProject createEclipseProject(Bundle bundle,
			String templateLocation, IProgressMonitor monitor)
			throws CoreException, IOException {

		IPath tplPrjDescr = new Path(templateLocation)
				.append(IProjectDescription.DESCRIPTION_FILE_NAME);
		IProjectDescription descr = ResourcesPlugin.getWorkspace()
				.loadProjectDescription(tplPrjDescr);
		descr.setLocation(null);
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				descr.getName());
		project.create(descr, monitor);
		project.open(IResource.BACKGROUND_REFRESH, monitor);

		return project;
	}
	
	public static IProject createEclipseProject(String name)
			throws CoreException, IOException {
		String descriptionText = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><projectDescription><name>{0}</name></projectDescription>";
		IProjectDescription descr = ResourcesPlugin.getWorkspace().loadProjectDescription(new StringBufferInputStream(MessageFormat.format(descriptionText,name)));
		descr.setLocation(null);
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(
				descr.getName());
		NullProgressMonitor nullProgress = new NullProgressMonitor();
		if(!project.exists()) {
			project.create(descr, nullProgress);
		}
		project.open(IResource.BACKGROUND_REFRESH, nullProgress);
		return project;
	}
	
	public static IProject createEclipseProject(Bundle bundle,
			String templateLocation)
			throws CoreException, IOException {

		return createEclipseProject(bundle,templateLocation, new NullProgressMonitor());
	}
	
	public static IProject createEclipseProject(String bundle,
			String templateLocation, IProgressMonitor monitor)
			throws CoreException, IOException {
		return createEclipseProject(
				Platform.getBundle(bundle), templateLocation, monitor);
	}
	
	public static boolean findLineInFile(IFile file, String pattern) throws CoreException, IOException {
		InputStream content = null;
		InputStreamReader isr = null;
		boolean patternIsFound = false;
		try {
			content = file.getContents(true);
			isr = new InputStreamReader(content);
			LineNumberReader contentReader = new LineNumberReader(isr);
			String line;
			do {
				line = contentReader.readLine();
				if(line!=null && !patternIsFound) {
					patternIsFound = line.trim().matches(pattern);
				}
			} while (line != null && !patternIsFound);
		}
		finally {
			if (isr != null) {
				try {
					isr.close();
				} catch (IOException e) {
					// ignore
				}
			}
			if (content != null) {
				try {
					content.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		return patternIsFound;
	}

	/**
	 * @param string
	 * @param string2
	 * @param nullProgressMonitor
	 * @return
	 * @throws InterruptedException 
	 * @throws InvocationTargetException 
	 * @throws CoreException 
	 * @throws IOException 
	 */
	public static IProject importProject(String bundleName, String templatePath,
			IProgressMonitor monitor) throws IOException, CoreException, InvocationTargetException, InterruptedException {
		Bundle bundle = Platform.getBundle(bundleName);
		if (bundle == null)
			return null;
		return importProject(bundle, templatePath, monitor==null?new NullProgressMonitor():monitor);
	}
	
	public static IProject importProject(String bundleName, String templatePath) throws IOException, CoreException, InvocationTargetException, InterruptedException {
		Bundle bundle = Platform.getBundle(bundleName);
		if (bundle == null)
			return null;
		return importProject(bundle, templatePath, null);
	}
	
	public static void deleteProject(String projectName) throws CoreException {
		IResource member = ResourcesPlugin.getWorkspace().getRoot().findMember(projectName);
		if (member != null) {
			try { 
				member.getProject().delete(true, true, null); 
			} catch (Exception e) {
				// Ignore any exceptions here (mostly because ResourceException rising is possible here)
				// But we cannot break tearDown() procedures in test cases which widely use this method
				// So, just print an exception stacktrace to see it in console log
				e.printStackTrace();
			}
		}
	}
	
	public static boolean setBuildAutomatically(boolean state)  throws CoreException {
	       boolean oldAutoBuilding;
	       IWorkspace workspace = ResourcesPlugin.getWorkspace();
	       IWorkspaceDescription description = workspace.getDescription();
	       oldAutoBuilding = description.isAutoBuilding();
	       if (state != oldAutoBuilding) {
	           description.setAutoBuilding(state);
	           workspace.setDescription(description);
	       }
	       return oldAutoBuilding;
	}

	//static public void importProjectIntoWorkspace(ImportBean bean) {
	//	importProjectIntoWorkspace(bean);
	//}

	private static final long IMPORT_DELAY = 1000;
	
	static public IProject importProjectIntoWorkspace(String path, String projectName) { 
		return importProjectIntoWorkspace(path, projectName,"file");
	}
	
	/**
	 * Import project into workspace.
	 * 
	 * @param path the path
	 * @param projectName the project name
	 * @param protocol 
	 */
	static public IProject importProjectIntoWorkspace(String path, String projectName, String protocol) {
	
	IProject project = null;
	
		try {
			 boolean state = ResourcesUtils.setBuildAutomatically(false);
			 project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			 if (!project.exists())
				 project.create(null);
			 project.open(null);
			 JobUtils.waitForIdle(IMPORT_DELAY);
			 
			 IOverwriteQuery overwrite = new IOverwriteQuery() {
				public String queryOverwrite(String pathString) {
					return ALL;
				}
			};
			if(Debug.DEBUG_IMPORT_OPERATION) {
				System.out.println("[ResourceUtils]:Target Import Location: " + project.getFullPath());
				System.out.println("[ResourceUtils]:Full path to source: " + path);
				System.out.println("[ResourceUtils]:Protocol: " + protocol);				
			}
			ImportOperation importOp = null;
			if("jar".equals(protocol)) {
				String pathToZip = path.substring(0,path.indexOf("!"));
				String zipEntryName = path.substring(path.indexOf("!") + 2, path.length());
				pathToZip = pathToZip.substring("file:".length());
				// create import operation
				if(Debug.DEBUG_IMPORT_OPERATION) {
					System.out.println("[ResourceUtils]:pathToZip: " + pathToZip);
					System.out.println("[ResourceUtils]:zipEntryName: " + zipEntryName);
				}
				ZipFile zipFile = new ZipFile(pathToZip);
				ZipFileStructureProvider zipStrProvider = new ZipFileStructureProvider(zipFile);
				ZipEntry source = getZipEntry(zipStrProvider,zipEntryName);
				importOp = new ImportOperation(project
						.getFullPath(), source, zipStrProvider , overwrite);
				importOp.setFilesToImport(zipStrProvider.getChildren(source));
			} else {
			ImportProvider importProvider = new ImportProvider();

			// need to remove from imported project "svn" files
			List<String> unimportedFiles = new ArrayList<String>();
			unimportedFiles.add(".svn"); //$NON-NLS-1$

			importProvider.setUnimportedFiles(unimportedFiles);
				// create import operation
				importOp = new ImportOperation(project
						.getFullPath(), new File(path), importProvider, overwrite);
			}
			
			// import files just to project folder ( without old structure )
			importOp.setCreateContainerStructure(false);

			importOp.setContext(PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getShell());
			// run import
			importOp.run(null);
			ResourcesUtils.setBuildAutomatically(state);
			
			JobUtils.waitForIdle(IMPORT_DELAY);
		} catch (InvocationTargetException ite) {
//			TePlugin.getDefault().logError(ite.getCause());
			ite.printStackTrace();
		} catch (InterruptedException ie) {
//			VPETestPlugin.getDefault().logError(ie);
			ie.printStackTrace();
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
		return project;
	}

	private static ZipEntry getZipEntry(
			ZipFileStructureProvider zipStrProvider, String zipEntryName) {
		String[] entries = zipEntryName.split("/");
		ZipEntry parent = zipStrProvider.getRoot();
		for (String string : entries) {
			List children = zipStrProvider.getChildren(parent);
			for (Object object : children) {
				ZipEntry current = (ZipEntry)object;
				String name = parent== zipStrProvider.getRoot()? string + "/": parent.getName() + string + "/";
				if(name.equals(current.getName())) {
					parent = current;
					break;
				}
			}
			
		}
		return parent;
	}

	public static IProject importProject(String bundleName, String projectPath,
			String name, Object object) throws IOException, CoreException, InvocationTargetException, InterruptedException {
		return importProject(Platform.getBundle(bundleName), projectPath, name, null);
	}
}
