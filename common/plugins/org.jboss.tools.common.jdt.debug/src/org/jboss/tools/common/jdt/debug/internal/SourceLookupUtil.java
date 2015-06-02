/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IPersistableSourceLocator;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourceLookupDirector;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.sourcelookup.containers.JavaProjectSourceContainer;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

public class SourceLookupUtil {
	public void addSelectedProjects(ILaunchConfigurationWorkingCopy wc, IJavaElement[] selection,
			IJavaProject javaProject) throws CoreException {
		if(( selection == null || selection.length == 0)) {
			// Nothing to do
			wc.setAttribute(RemoteDebugActivator.ATTR_SELECTED_PROJECTS, (List<String>)null);
		} else {
			ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
			ISourceLookupDirector director = addSourceContainers(manager, wc, selection, javaProject);
			if (director != null) {
				wc.setAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_MEMENTO, director.getMemento());
				wc.setAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_ID, director.getId());
			}
		}
	}
	
	private ISourceLookupDirector addSourceContainers(ILaunchManager manager,
			ILaunchConfigurationWorkingCopy wc, IJavaElement[] selection, IJavaProject javaProject) throws CoreException {
		String memento = null;
		String locatorId = null;
		try {
			memento = wc.getAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_MEMENTO,(String) null);
			locatorId = wc.getAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_ID,(String) null);
			if (locatorId == null) {
				locatorId = wc.getType().getSourceLocatorId();
			}
		} catch (CoreException e) {
			RemoteDebugActivator.pluginLog().logError(e);
		}
		IPersistableSourceLocator locator = manager.newSourceLocator(locatorId);
		if (memento != null) {
			locator.initializeFromMemento(memento);
		} else {
			locator.initializeDefaults(wc);
		}
		if (locator instanceof AbstractSourceLookupDirector) {
			ISourceLookupDirector director = (ISourceLookupDirector) locator;
			ArrayList<ISourceContainer> sourceContainers = new ArrayList<ISourceContainer>(Arrays.asList(director.getSourceContainers()));
			Set<String> projectNames = new LinkedHashSet<String>();
			if( javaProject != null ) {
				projectNames.add(javaProject.getElementName());
			}
			for( int i = 0; i < selection.length; i++ ) {
				IJavaProject project = selection[i].getJavaProject();
				if (project instanceof IJavaProject && !project.equals(javaProject)) {
					projectNames.add( ((IJavaProject) project).getElementName());
					sourceContainers.add(new JavaProjectSourceContainer((IJavaProject)project));
				}
			}
			
			director.setSourceContainers((ISourceContainer[]) sourceContainers.toArray(new ISourceContainer[sourceContainers.size()]));
			director.setFindDuplicates(true);
			List<String> projectsList = new ArrayList<String>();
			projectsList.addAll(projectNames);
			wc.setAttribute(RemoteDebugActivator.ATTR_SELECTED_PROJECTS, projectsList);
			wc.setAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_MEMENTO, director.getMemento());
			wc.setAttribute(ILaunchConfiguration.ATTR_SOURCE_LOCATOR_ID, director.getId());
			return director;
		} else {
			RemoteDebugActivator.pluginLog().logWarning("Launch configuration doesn't support source lookup");
		}
		return null;
	}
}
