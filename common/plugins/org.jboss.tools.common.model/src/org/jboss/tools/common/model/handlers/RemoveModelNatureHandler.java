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
package org.jboss.tools.common.model.handlers;

import java.io.*;
import java.text.MessageFormat;
import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.eclipse.jdt.core.*;
import org.jboss.tools.common.meta.action.SignificanceMessageFactory;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.meta.action.impl.XActionImpl;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.project.ModelNatureExtension;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.FileUtil;

public class RemoveModelNatureHandler extends AbstractHandler {
	public static String PARAM_CONTRIBUTION = "contribution"; //$NON-NLS-1$
	
	public boolean isEnabled(XModelObject object) {
		if(object == null) return false;
		String nature = getNatureDisplayName(object);
		if(nature == null) return false;
		XActionImpl i = (XActionImpl)action;
		i.setDisplayName(MessageFormat.format("Remove {0} Capabilities", nature));
		return true;
	}
	
	private String getNatureDisplayName(XModelObject object) {
		IProject p = EclipseResourceUtil.getProject(object);
		ModelNatureExtension[] es = ModelNatureExtension.getInstances();
		for (ModelNatureExtension ext: es) {
			String name = ext.getName();
			if(p != null && p.isAccessible()) try {
				if(p.hasNature(name)) {
					return ext.getDisplayName();
				}
			} catch (CoreException e) {
				
			}
		}
		return null;		
	}

	private String getNature(XModelObject object) {
		IProject p = EclipseResourceUtil.getProject(object);
		ModelNatureExtension[] es = ModelNatureExtension.getInstances();
		for (ModelNatureExtension ext: es) {
			String name = ext.getName();
			if(p != null && p.isAccessible()) try {
				if(p.hasNature(name)) {
					return ext.getName();
				}
			} catch (CoreException e) {
				
			}
		}
		return null;		
	}

	public void executeHandler(XModelObject object, Properties p) throws XModelException {
		IProject project = EclipseResourceUtil.getProject(object);
		String nature = (p == null) ? null : p.getProperty("nature"); //$NON-NLS-1$
		if(nature == null) nature = getNature(object);
		if (project == null || nature == null) return;
		
		boolean unregisterWTP = false;

		ServiceDialog dialog = object.getModel().getService();
		Properties pd = new Properties();
		String message = MessageFormat.format("Remove {0} Capabilities from project {1}?", getNatureDisplayName(object), project.getName());
			//SignificanceMessageFactory.getInstance().getMessage(action, object, null) + "?";
		pd.setProperty(ServiceDialog.DIALOG_MESSAGE, message);
		String checkBoxMessage = "Remove Dynamic Web Project Capabilities";
		pd.setProperty(ServiceDialog.CHECKBOX_MESSAGE, checkBoxMessage);
		pd.put(ServiceDialog.CHECKED, Boolean.FALSE);
		if(!dialog.openConfirm(pd)) return;

		SpecialWizard contribution = p == null ? null : (SpecialWizard)p.get(PARAM_CONTRIBUTION);
		if(contribution != null) {
			contribution.setObject(object.getModel());
			contribution.execute();
		}
		Boolean b = (Boolean)pd.get(ServiceDialog.CHECKED);
		unregisterWTP = b.booleanValue();
		
		try {
			IProjectDescription d = project.getDescription();
			String[] ns = d.getNatureIds();
			String[] ns2 = removeNature(ns, nature);
			if(unregisterWTP) ns2 = new String[]{JavaCore.NATURE_ID};
			if(ns.length == ns2.length) return;
			if(unregisterWTP) {
				unregisterFromServer(object);
				clearClassPath(project);
			}
			d.setNatureIds(ns2);
			project.setDescription(d, IResource.FORCE, null);
			if(EclipseResourceUtil.getModelNature(project) != null) return;
			String projectLocation = project.getLocation().toString();
			removeFiles(projectLocation, XModelConstants.getWorkspace(object.getModel()));
			if(unregisterWTP) {
				File f = new File(projectLocation + "/.settings"); //$NON-NLS-1$
				if(f.isDirectory()) FileUtil.remove(f);
			}
			clear(object.getModel().getByPath("FileSystems/WEB-INF")); //$NON-NLS-1$
			project.refreshLocal(IResource.DEPTH_INFINITE, null);
		} catch (CoreException e) {
			throw new XModelException(e);
		}
	}
	
	private void removeFiles(String location, String workspace) {
		File f = new File(location + XModelObjectConstants.SEPARATOR + IModelNature.PROJECT_FILE);
		if(f.exists()) f.delete();
	}

	public boolean getSignificantFlag(XModelObject object) {
		return false;
	}
	
	public String[] removeNature(String[] natures, String nature) {
		ArrayList<String> l = new ArrayList<String>();
		for (int i = 0; i < natures.length; i++)
		  if(!natures[i].equals(nature)) l.add(natures[i]);
		return l.toArray(new String[0]);
	}

///MarkerClearer
	public static void clear(XModelObject o) {
		if(o == null || o.getFileType() == XModelObject.NONE) return;
		if(o.getFileType() == XModelObject.FILE) {
			ResourceMarkers markers = new ResourceMarkers(ResourceMarkers.JST_WEB_PROBLEM);
			markers.setModelObject(o);
			markers.update();
		} else {
			XModelObject[] os = o.getChildren();
			for (int i = 0; i < os.length; i++) clear(os[i]); 
		}
	}
	
// Remove Dynamic Web Project Capabilities
	
	void clearClassPath(IProject project) throws XModelException {
		IJavaProject javaProject = JavaCore.create(project);
		ArrayList<IClasspathEntry> newClassPath = new ArrayList<IClasspathEntry>(getRawClassPath(javaProject));
		Iterator<IClasspathEntry> iterator = newClassPath.iterator();
		while (iterator.hasNext()) {
			IClasspathEntry entry = iterator.next();
			if(entry.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
				if(entry.getPath().toString().indexOf(".jst.") >= 0) { //$NON-NLS-1$
					iterator.remove();
					continue;
				}
			}
			if (project.getFullPath().equals(entry.getPath())) iterator.remove();  	
		}
		IClasspathEntry[] entries = newClassPath.toArray(new IClasspathEntry[newClassPath.size()]);
		if(entries.length != 0) {
			try {
				javaProject.setRawClasspath(entries, new NullProgressMonitor());
			} catch (JavaModelException e) {
				throw new XModelException(e);
			}
		}
	}
	
	private List<IClasspathEntry> getRawClassPath(IJavaProject javaProject) throws XModelException {
		try {
			return Arrays.asList(javaProject.getRawClasspath());
		} catch (JavaModelException e) {
			throw new XModelException(e);
		}
	}
	
	void unregisterFromServer(XModelObject object) {
		XModelObject fs = FileSystemsHelper.getFileSystems(object.getModel());
		XAction action = XActionInvoker.getAction("Registration.UnregisterInServerXML", fs); //$NON-NLS-1$
		if(action != null) {
			Properties runningProperties = new Properties();
			runningProperties.setProperty("unregisterFromAllServers", XModelObjectConstants.TRUE); //$NON-NLS-1$
			runningProperties.setProperty("showResult", XModelObjectConstants.FALSE); //$NON-NLS-1$
			XActionInvoker.invoke("Registration.UnregisterInServerXML", fs, runningProperties); //$NON-NLS-1$
		}
	}

}
