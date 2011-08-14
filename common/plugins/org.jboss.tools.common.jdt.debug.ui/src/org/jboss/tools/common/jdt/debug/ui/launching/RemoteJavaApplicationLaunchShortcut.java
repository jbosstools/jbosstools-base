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
package org.jboss.tools.common.jdt.debug.ui.launching;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ITypeRoot;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.jdt.debug.VmModel;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.jboss.tools.common.jdt.debug.ui.actions.RemoteLaunchAction;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteJavaApplicationLaunchShortcut implements ILaunchShortcut2 {
	
	private int retValue;
	private boolean restarted; 

	/* (non-Javadoc)
	 * @see ILaunchShortcut#launch(ISelection, String)
	 */
	public void launch(ISelection selection, final String mode) {
		executeAction(getLaunchableResource(selection));
	}

	private void executeAction(final IResource resource) {
		final boolean oldDiscover = RemoteDebugUIActivator.getDefault().isDiscoverRemoteApplication();
		final IEclipsePreferences preferences = RemoteDebugUIActivator.getDefault().getPreferences();
		preferences.putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, true);
		
		final Job job = RemoteDebugUIActivator.getDefault().getRemoteApplicationJob();
		while (job.getState() != Job.NONE) {
			restarted = true;
			MessageDialog dialog = new MessageDialog(getShell(), 
					"Remote Java Application scan already in progress", 
					null, 
					"Remote Java Application scan already in progress.\nPlease wait for it to be complete.", 
					MessageDialog.QUESTION, new String[] {"OK", "Try again"}, 0);
			retValue = dialog.open();
			if (retValue == 0) {
				restarted = false;
				return;
			}
		}
		restarted = false;
		job.addJobChangeListener(new IJobChangeListener() {
			
			@Override
			public void sleeping(IJobChangeEvent event) {
			}
			
			@Override
			public void scheduled(IJobChangeEvent event) {
				
			}
			
			@Override
			public void running(IJobChangeEvent event) {
				
			}
			
			@Override
			public void done(IJobChangeEvent event) {
				preferences.putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, oldDiscover);
				job.removeJobChangeListener(this);
				if (restarted) {
					final Display display = Display.getDefault();
					while (restarted) {
						display.syncExec(new Runnable() {
							
							@Override
							public void run() {
								if (!display.readAndDispatch()) {
									display.sleep();
								}
							}
						});
					}
					if (retValue != 0) {
						return;
					}
				}
				Display.getDefault().asyncExec(new Runnable() {
					
					@Override
					public void run() {
						showDialog(resource);
					}

				});
			}
			
			@Override
			public void awake(IJobChangeEvent event) {
				
			}
			
			@Override
			public void aboutToRun(IJobChangeEvent event) {
				
			}
		});
		job.schedule();
	}

	/* (non-Javadoc)
	 * @see ILaunchShortcut#launch(IEditorPart, String)
	 */
	public void launch(IEditorPart editor, String mode) {
		executeAction(getLaunchableResource(editor));
	}

	public ILaunchConfiguration[] getLaunchConfigurations(ISelection selection) {
		return new ILaunchConfiguration[0];
	}

	/* (non-Javadoc)
	 * @see ILaunchShortcut2#getLaunchConfigurations(IEditorPart)
	 */
	public ILaunchConfiguration[] getLaunchConfigurations(IEditorPart editor) {
		return new ILaunchConfiguration[0];
	}

	public IResource getLaunchableResource(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;
			if (ss.size() == 1) {
				Object selected = ss.getFirstElement();
				if (!(selected instanceof IJavaElement)
						&& selected instanceof IAdaptable) {
					selected = ((IAdaptable) selected)
							.getAdapter(IJavaElement.class);
				}
				if (selected instanceof IJavaElement) {
					return ((IJavaElement) selected).getResource();
				}
			}
		}
		return null;
	}

	public IResource getLaunchableResource(IEditorPart editor) {
		ITypeRoot element = JavaUI.getEditorInputTypeRoot(editor
				.getEditorInput());
		if (element != null) {
			try {
				return element.getCorrespondingResource();
			} catch (JavaModelException e) {
			}
		}
		return null;
	}
	
	private void showDialog(IResource resource) {
		VmModel[] vmModels = RemoteDebugUIActivator.getDefault().getCurrentDebugModels();
		if (vmModels == null || vmModels.length == 0) {
			boolean question = MessageDialog.openQuestion(getShell(), 
					"No remote Java ApplicationFound", 
					"Sorry, could not detect any running remote java applications.\nTry search again?");
			if (question) {
				executeAction(resource);
			}
		} else {
			boolean autoConnect = RemoteDebugUIActivator.getDefault().isAutoConnect();
			if (autoConnect && vmModels.length == 1) {
				new RemoteLaunchAction(vmModels[0].getPort()).run();
			} else {
				Dialog dialog = new LaunchRemoteApplicationDialog(getShell());
				dialog.open();
			}
		}
	}
	
	private static Shell getShell() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window == null) {
			IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
			if (windows.length > 0) {
				return windows[0].getShell();
			}
		}
		else {
			return window.getShell();
		}
		return null;
	}
	
}