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
import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.eclipse.jdt.core.*;
import org.jboss.tools.common.meta.action.SignificanceMessageFactory;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.meta.action.impl.XActionImpl;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.FileUtil;

public class RemoveModelNatureHandler extends AbstractHandler {
	
	public boolean isEnabled(XModelObject object) {
		if(object == null) return false;
		String nature = getNature(object);
		if(nature == null) return false;
		XActionImpl i = (XActionImpl)action;
		String n = (nature.indexOf("struts") > 0) ? "Struts" :
		           (nature.indexOf("jsf") > 0) ? "JSF" : "Model"; 
		i.setDisplayName("Remove " + n + " Capabilities");
		return true;
	}
	
	private IProject getProject(XModelObject object) {
		return (object == null) ? null : (IProject)object.getModel().getProperties().get("project");
	}
	
	private String getNature(XModelObject object) {
		IProject p = getProject(object);
		IModelNature n = EclipseResourceUtil.getModelNature(p);
		return n == null ? null : n.getID();		
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		IProject project = getProject(object);
		String nature = (p == null) ? null : p.getProperty("nature"); 
		if(nature == null) nature = getNature(object);
		if (project == null || nature == null) return;
		
		boolean unregisterWTP = false;

		ServiceDialog dialog = object.getModel().getService();
		Properties pd = new Properties();
		String message = SignificanceMessageFactory.getInstance().getMessage(action, object, null) + "?";
		pd.setProperty(ServiceDialog.DIALOG_MESSAGE, message);
		String checkBoxMessage = "Remove Dynamic Web Project Capabilities";
		pd.setProperty(ServiceDialog.CHECKBOX_MESSAGE, checkBoxMessage);
		pd.put(ServiceDialog.CHECKED, new Boolean(false));
		if(!dialog.openConfirm(pd)) return;
		Boolean b = (Boolean)pd.get(ServiceDialog.CHECKED);
		unregisterWTP = b.booleanValue();
		
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
			File f = new File(projectLocation + "/.settings");
			if(f.isDirectory()) FileUtil.remove(f);
		}
		clear(object.getModel().getByPath("FileSystems/WEB-INF"));
		project.refreshLocal(IResource.DEPTH_INFINITE, null);
	}
	
	private void removeFiles(String location, String workspace) {
		File f = new File(location + "/" + IModelNature.PROJECT_FILE);
		if(f.exists()) f.delete();
		File dir = new File(workspace);
		File[] fs = dir.listFiles();
		if(fs != null) for (int i = 0; i < fs.length; i++) 
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
	
	void clearClassPath(IProject project) throws Exception {
		IJavaProject javaProject = JavaCore.create(project);
		ArrayList<IClasspathEntry> newClassPath = new ArrayList<IClasspathEntry>(Arrays.asList(javaProject.getRawClasspath()));
		Iterator<IClasspathEntry> iterator = newClassPath.iterator();
		while (iterator.hasNext()) {
			IClasspathEntry entry = iterator.next();
			if(entry.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
				if(entry.getPath().toString().indexOf(".jst.") >= 0) {
					iterator.remove();
					continue;
				}
			}
			if (project.getFullPath().equals(entry.getPath())) iterator.remove();  	
		}
		IClasspathEntry[] entries = newClassPath.toArray(new IClasspathEntry[newClassPath.size()]);
		if(entries.length != 0) {
			javaProject.setRawClasspath(entries, new NullProgressMonitor());
		}
	}
	
	void unregisterFromServer(XModelObject object) {
		XModelObject fs = FileSystemsHelper.getFileSystems(object.getModel());
		XAction action = XActionInvoker.getAction("Registration.UnregisterInServerXML", fs);
		if(action != null) {
			Properties runningProperties = new Properties();
			runningProperties.setProperty("unregisterFromAllServers", "true");
			runningProperties.setProperty("showResult", "false");
			XActionInvoker.invoke("Registration.UnregisterInServerXML", fs, runningProperties);
		}
	}

}
