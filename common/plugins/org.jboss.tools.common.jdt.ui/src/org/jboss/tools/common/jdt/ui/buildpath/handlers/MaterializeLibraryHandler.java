/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.ui.buildpath.handlers;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceRuleFactory;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.packageview.ClassPathContainer;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.jboss.tools.common.jdt.core.buildpath.MaterializeLibraryJob;
import org.jboss.tools.common.jdt.ui.buildpath.dialog.MaterializeLibraryDialog;

/**
 * Materialize Library Handler.
 * 
 * @author Fred Bricon
 */

@SuppressWarnings("restriction")
public class MaterializeLibraryHandler extends AbstractHandler {

  @Override
  public Object execute(final ExecutionEvent event) throws ExecutionException {

    ISelection selection = HandlerUtil.getCurrentSelection(event);
    ClassPathContainer libraryFromUI = getSelectedLibrary(selection);

    if (libraryFromUI != null) {
      
      IPath path = libraryFromUI.getClasspathEntry().getPath();
      
      IJavaProject javaProject = libraryFromUI.getJavaProject();
      
      final String libName = libraryFromUI.getLabel();
      
      try {
        IClasspathContainer containerToMaterialize = JavaCore.getClasspathContainer(path, javaProject);
      
        IProject project = javaProject.getProject();
        
        final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
        
        MaterializeLibraryDialog dialog = new MaterializeLibraryDialog(window.getShell(), 
                                                                       project,
                                                                       containerToMaterialize, 
                                                                       getDefaultLib(libraryFromUI.getClasspathEntry()) 
                                                                       ); 
        if(dialog.open() == Dialog.OK) {
          Map<IPath, String> jarsToMaterialize = dialog.getSelectedClasspathEntryPaths();
  
          IFolder libFolder = dialog.getLibFolder();
          
          Job job = new MaterializeLibraryJob(javaProject, 
                                              containerToMaterialize,
                                              jarsToMaterialize, 
                                              libFolder);
          job.setRule(getRule(project));
          job.addJobChangeListener(new IJobChangeListener() {
			
			@Override
			public void sleeping(IJobChangeEvent arg0) {
			}
			
			@Override
			public void scheduled(IJobChangeEvent arg0) {
			}
			
			@Override
			public void running(IJobChangeEvent arg0) {
			}
			
			@Override
			public void done(IJobChangeEvent changeEvent) {
				final IStatus result = changeEvent.getResult();
				if (IStatus.OK != result.getCode()) {
					Display.getDefault().asyncExec(new Runnable() {
						public void run() {
							Shell shell = HandlerUtil.getActiveShell(event);
							Throwable e = result.getException();
							if (e != null) e.printStackTrace();
							MessageDialogWithToggle.openError(shell, NLS.bind("Error Materializing {0}", libName), result.getMessage()); 
						}
					});
				}
			}
			
			@Override
			public void awake(IJobChangeEvent arg0) {
			}
			
			@Override
			public void aboutToRun(IJobChangeEvent arg0) {
			}
          });
          job.schedule(); 
        }

      } catch (JavaModelException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

    }
    return null;
  }

  private String getDefaultLib(IClasspathEntry classpathEntry) { 
    String defaultLib = "lib";
    //TODO look for the value of IClasspathAttribute "org.eclipse.jst.component.dependency"
    //TODO Even if we get WEB-INF/lib from IClasspathAttribute, how do we get the first mapped source folder 
    //without depending on the component fwk? extension points (and new component adapter plugin)? reflection?  
    return defaultLib;
  }

  private ISchedulingRule getRule(IProject project) {
    IResourceRuleFactory ruleFactory = ResourcesPlugin.getWorkspace().getRuleFactory();
    ISchedulingRule rule = ruleFactory.buildRule();
    return rule;
  }

  private ClassPathContainer getSelectedLibrary(ISelection selection) {
    ClassPathContainer container = null;
    if (selection instanceof IStructuredSelection) {
      IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      Object o = structuredSelection.getFirstElement();
      if (o instanceof ClassPathContainer) {
        container = (ClassPathContainer) o;
      }
    }
    return container;
  }
}






















