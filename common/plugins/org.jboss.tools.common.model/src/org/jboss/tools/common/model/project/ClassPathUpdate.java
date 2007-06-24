/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.project;

import java.io.File;
import java.util.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;
import org.eclipse.jdt.core.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.FileSystemsLoader;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.*;

public class ClassPathUpdate {
	IProject project = null;
	IJavaProject javaProject;
	XModel model;
	List<IClasspathEntry> newClassPath; 
	IPath classes;
	Map<String,String> replacedSrc = new HashMap<String,String>();
	
	public void setProject(IProject project) {
		this.project = project;
	}
	
	public void setModel(XModel model) {
		String entity = model.getRoot().getModelEntity().getName();
		if("RootDummy".equals(entity)) {
			String pn = (project == null) ? "?" : project.getName();
			throw new RuntimeException("Struts Project " + pn + " is corrupted.");
		}
		this.model = model;
	}
	
	public XModel getModel() {
		return model;
	}
	
	public void setReplacedSrc(Map<String,String> replacedSrc) {
		this.replacedSrc = replacedSrc;
	}
	
	public void setClasses(IPath classes) {
		this.classes = classes;
	}
	
	public void execute() throws CoreException {
		if(model == null) {
			IModelNature nature = (IModelNature)EclipseResourceUtil.getModelNature(project);
			setModel(nature.getModel());
		}
		javaProject = JavaCore.create(project);
		EclipseResourceUtil.addNatureToProject(project, JavaCore.NATURE_ID);
		init();
		createOutputFolder();		
		removeReplacedSrc();
		createSrcFolders();		
		createClassPath();
		commit();
	}
	
	private void init() throws CoreException {
		newClassPath = new ArrayList<IClasspathEntry>(Arrays.asList(javaProject.getRawClasspath()));
		Iterator iterator = newClassPath.iterator();
		while (iterator.hasNext()) {
			IClasspathEntry entry = (IClasspathEntry)iterator.next();
			if (project.getFullPath().equals(entry.getPath())) iterator.remove();  	
		}
	}
	
	private void commit() throws CoreException {
		removeDuplicateEntries();
		IClasspathEntry[] entries = (IClasspathEntry[])newClassPath.toArray(new IClasspathEntry[newClassPath.size()]);
		if(entries.length != 0) {
			javaProject.setRawClasspath(entries, new NullProgressMonitor());
		}
	}
	
	public void revalidateLibs(XModel model) throws Exception {
		setProject(EclipseResourceUtil.getProject(model.getRoot()));
		setModel(model);
		javaProject = JavaCore.create(project);
		init();
		addLibJars();
		commit();
	}
	
	private void removeReplacedSrc() {
		if(replacedSrc == null || replacedSrc.isEmpty()) return;
		String[] s = (String[])replacedSrc.keySet().toArray(new String[0]);
		for (int i = 0; i < s.length; i++) {
			IClasspathEntry entry = createNewClasspathEntry(s[i], IClasspathEntry.CPE_SOURCE);
			int i1 = newClassPath.indexOf(entry);
			if(i1 < 0) continue;
			String sn = (String)replacedSrc.get(s[i]);
			if(sn == null || sn.length() == 0) {
				newClassPath.remove(i1);
			} else {
				entry = createNewClasspathEntry(sn, IClasspathEntry.CPE_SOURCE);
				int i2 = newClassPath.indexOf(entry);
				if(i2 < 0) newClassPath.set(i1, entry); else newClassPath.remove(i1);
			}
		} 
	}
	
	private void createSrcFolders() {
		XModelObject web = model.getByPath("Web");
		if(web == null) return;
		XModelObject[] children = model.getByPath("Web").getChildren();
		List<String> srcPaths = new ArrayList<String>();
		Set<String> set = new HashSet<String>();
		for (int i = 0; i < children.length; i++) {
			String objectPaths = children[i].getAttributeValue("src file system"/*WebModuleConstants.ATTR_SRC_FS*/);
			if(objectPaths == null || objectPaths.length() == 0) continue;
			StringTokenizer st = new StringTokenizer(objectPaths, ",;");
			while(st.hasMoreTokens()) {
				String objectPath = st.nextToken().trim();
				if(objectPath.length() == 0) continue;
				XModelObject srcObject = model.getByPath("FileSystems/" + objectPath);
				if (srcObject == null) continue;
				String osPath = XModelObjectUtil.getExpandedValue(srcObject, "location", null);
				if (osPath != null && !"".equals(osPath) && !set.contains(osPath)) {
					srcPaths.add(osPath);
					set.add(osPath);
				}					 
			}
		}

		Iterator iterator = srcPaths.iterator();
		while (iterator.hasNext()) {
			IClasspathEntry entry = createNewClasspathEntry(iterator.next().toString(), IClasspathEntry.CPE_SOURCE);
			insertSrc(entry);
		} 
	}
	
	private void insertSrc(IClasspathEntry entry) {
		if(!entryAlreadyExists(entry, newClassPath)) newClassPath.add(0, entry);
	}
	
	private void createOutputFolder() {
		XModelObject classesObject = model.getByPath("FileSystems/classes");
		String classesPath = "";
		if (classesObject != null)
			classesPath = XModelObjectUtil.getExpandedValue(classesObject, "location", null);
		else if(classes != null)
			classesPath = classes.toString();

		try {
			javaProject.setOutputLocation(
				EclipseResourceUtil.getRelativePath(project, classesPath),
				new NullProgressMonitor()
			);
		} catch(JavaModelException ex) {
			ModelPlugin.log(ex);
		}
	}
	
	private void removeDuplicateEntries() {
		List<IClasspathEntry> removeList = new ArrayList<IClasspathEntry>();
		List<IClasspathEntry> tmpList = new ArrayList<IClasspathEntry>();	
		Iterator<IClasspathEntry> iterator = newClassPath.iterator();
		while (iterator.hasNext()) {
			IClasspathEntry entry = (IClasspathEntry)iterator.next();
			tmpList.clear();
			tmpList.addAll(newClassPath);
			tmpList.remove(entry);
			if (entryAlreadyExists(entry, tmpList)) removeList.add(entry);
		}
		iterator = removeList.iterator();
		while (iterator.hasNext()) newClassPath.remove(iterator.next());
	}
	
	private void addJRE() {
		IClasspathEntry[] jre = EclipseResourceUtil.getDefaultJRELibrary();
		for (int i = 0; i < jre.length; i++)
			if (!entryAlreadyExists(jre[i], newClassPath)) newClassPath.add(jre[i]);
	}
	
	private void addLibJars() {
		XModelObject fss = model.getByPath("FileSystems");
		XModelObject[] children = fss == null ? new XModelObject[0] : fss.getChildren("FileSystemJar");
		List<String> srcPaths = new ArrayList<String>();
		for (int i = 0; i < children.length; i++) {
			if("true".equals(children[i].get(FileSystemsLoader.IS_ADDED_TO_CLASSPATH))) continue;
			String osPath = XModelObjectUtil.getExpandedValue(children[i], "location", null);
			if (osPath != null && !"".equals(osPath) && new File(osPath).isFile()) srcPaths.add(osPath);
			children[i].set(FileSystemsLoader.IS_ADDED_TO_CLASSPATH, "true");
		}

// Leave it to WTP
//		Iterator iterator = srcPaths.iterator();
//		while (iterator.hasNext()) {
//			IClasspathEntry entry = createNewClasspathEntry(iterator.next().toString(), IClasspathEntry.CPE_LIBRARY); 
//			if (!entryAlreadyExists(entry, newClassPath)) newClassPath.add(entry);
//		} 
	}
	
	private void createClassPath() {
		addJRE();
		addLibJars();		
		addServletSupport();
	}
	
	static SpecialWizard addServletSupportWizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.jst.web.project.AddServletSupportWizard");
	private void addServletSupport() {
//		if(addServletSupportWizard == null) return;
//		addServletSupportWizard.setObject(this);
//		addServletSupportWizard.execute();
	}

	private boolean entryAlreadyExists(IClasspathEntry entry, List entryList) {
		boolean result = (entry != null && entryList.contains(entry));
		if(result) return true;
		IClasspathEntry resolvedEntry = JavaCore.getResolvedClasspathEntry(entry);
		IPath resolvedEntryPath = (resolvedEntry != null) ? resolvedEntry.getPath() : null;
		if (resolvedEntryPath == null) return false;
		Iterator iterator = entryList.iterator();
		while (iterator.hasNext() && !result) {
			IClasspathEntry tmpEntry = (IClasspathEntry)iterator.next();
			IClasspathEntry tmpResolvedEntry;
			if (tmpEntry.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
				IClasspathContainer container; 
				try {
					container = JavaCore.getClasspathContainer(tmpEntry.getPath(), javaProject);
				} catch (JavaModelException e) {
					container = null;
				}
				if (container == null) continue;
				IClasspathEntry containerEntries[] = container.getClasspathEntries();
				for (int i = 0; i < containerEntries.length && !result; i++) {
					tmpResolvedEntry = JavaCore.getResolvedClasspathEntry(containerEntries[i]);
					result = (tmpResolvedEntry != null && resolvedEntryPath.equals(tmpResolvedEntry.getPath()));
				}
			} else {
				tmpResolvedEntry = JavaCore.getResolvedClasspathEntry(tmpEntry);
				result = (tmpResolvedEntry != null && resolvedEntryPath.equals(tmpResolvedEntry.getPath()));
			}
		}
		if(entry.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
			IClasspathContainer container; 
			try {
				container = JavaCore.getClasspathContainer(entry.getPath(), javaProject);
			} catch (JavaModelException e) {
				container = null;
			}
			if (container != null) {
				IClasspathEntry[] containerEntries = container.getClasspathEntries();
				for (int i = 0; i < containerEntries.length; i++) {
					if(entryAlreadyExists(containerEntries[i], entryList)) return true;
				}
			}
		}
		return result;
	}

	private IClasspathEntry createNewClasspathEntry(String location, int entryKind) {
		IPath entryPath = EclipseResourceUtil.getRelativePath(project, location);
		return createNewClasspathEntry(entryPath, entryKind); 
	}	

	public IClasspathEntry createNewClasspathEntry(IPath path, int entryKind) {
		switch(entryKind) {
			case IClasspathEntry.CPE_SOURCE:
				return JavaCore.newSourceEntry(path);
			case IClasspathEntry.CPE_LIBRARY:
				return JavaCore.newLibraryEntry(path, null, null);
			case IClasspathEntry.CPE_VARIABLE:
				return JavaCore.newVariableEntry(path, null, null);	
			case IClasspathEntry.CPE_CONTAINER:
				return JavaCore.newContainerEntry(path);
			default:
				return null;	
		}
	}
	
	public void registerEntry(IClasspathEntry entry) {
		if (!entryAlreadyExists(entry, newClassPath)) newClassPath.add(entry);
	}
	
}
