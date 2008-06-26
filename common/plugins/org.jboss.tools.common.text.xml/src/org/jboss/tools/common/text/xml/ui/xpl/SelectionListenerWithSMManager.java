/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.common.text.xml.ui.xpl;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.ui.StructuredTextEditor;

/**
 * @author Jeremy
 *
 * Infrastructure to share an AST for editor post selection listeners.
 */
public class SelectionListenerWithSMManager {
	
	private static SelectionListenerWithSMManager fgDefault;
	
	/**
	 * @return Returns the default manager instance.
	 */
	public static SelectionListenerWithSMManager getDefault() {
		if (fgDefault == null) {
			fgDefault= new SelectionListenerWithSMManager();
		}
		return fgDefault;
	}
	
	
	private final static class PartListenerGroup {
		private ITextEditor fPart;
		private ISelectionChangedListener fSelectionListener, fPostSelectionListener;
		private Job fCurrentJob;
		private ListenerList fSMListeners;
		
		public PartListenerGroup(ITextEditor part) {
			fPart= part;
			fCurrentJob= null;
			fSMListeners= new ListenerList();
			
			fSelectionListener= new ISelectionChangedListener() {
				public void selectionChanged(SelectionChangedEvent event) {
					ISelection selection= event.getSelection();
					if (selection instanceof ITextSelection) {
						fireSelectionChanged((ITextSelection) selection);
					}
				}
			};
			
			fPostSelectionListener= new ISelectionChangedListener() {
				public void selectionChanged(SelectionChangedEvent event) {
					ISelection selection= event.getSelection();
					if (selection instanceof ITextSelection) {
						firePostSelectionChanged((ITextSelection) selection);
					}
				}
			};
		}

		public boolean isEmpty() {
			return fSMListeners.isEmpty();
		}

		public void install(ISelectionListenerWithSM listener) {
			if (isEmpty()) {
				ISelectionProvider selectionProvider= fPart.getSelectionProvider();
				if (selectionProvider instanceof IPostSelectionProvider) {
					((IPostSelectionProvider) selectionProvider).addPostSelectionChangedListener(fPostSelectionListener);
					selectionProvider.addSelectionChangedListener(fSelectionListener);
				}
			}
			fSMListeners.add(listener);
		}
		
		public void uninstall(ISelectionListenerWithSM listener) {
			fSMListeners.remove(listener);
			if (isEmpty()) {
				ISelectionProvider selectionProvider= fPart.getSelectionProvider();
				if (selectionProvider instanceof IPostSelectionProvider) {
					((IPostSelectionProvider) selectionProvider).removePostSelectionChangedListener(fPostSelectionListener);
					selectionProvider.removeSelectionChangedListener(fSelectionListener);
				}
			}
		}
		
		public void fireSelectionChanged(final ITextSelection selection) {
			if (fCurrentJob != null) {
				fCurrentJob.cancel();
			}
		}
		
		public void firePostSelectionChanged(final ITextSelection selection) {
			if (fCurrentJob != null) {
				fCurrentJob.cancel();
			}
			final IStructuredModel model= getStructuredModel();
			if (model == null) {
				return;
			}
			
			fCurrentJob= new Job(UIMessages.getString("SelectionListenerWithSMManager.job.title")) { //$NON-NLS-1$
				public IStatus run(IProgressMonitor monitor) {
					if (monitor == null) {
						monitor= new NullProgressMonitor();
					}
					synchronized (PartListenerGroup.this) {
						return inform(model, selection, monitor);
					}
				}
			};
			fCurrentJob.setPriority(Job.DECORATE);
			fCurrentJob.setSystem(true);
			fCurrentJob.schedule();
		}
		
		private IStructuredModel getStructuredModel() {
			if (fPart instanceof StructuredTextEditor) {
				return ((StructuredTextEditor)fPart).getModel();
			} else
				return null;
		}
		
		protected IStatus inform(IStructuredModel model, ITextSelection selection, IProgressMonitor monitor) {
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			// Probably should create read-only copy of IStructureModel object
			try {
//--				CompilationUnit astRoot= JavaPlugin.getDefault().getASTProvider().getAST(input, true, monitor);
			
				if ( model != null && !monitor.isCanceled()) {
					Object[] listeners= fSMListeners.getListeners();
					for (int i= 0; i < listeners.length; i++) {
						((ISelectionListenerWithSM) listeners[i]).selectionChanged(fPart, selection, model);
						if (monitor.isCanceled()) {
							return Status.CANCEL_STATUS;
						}
					}
					return Status.OK_STATUS;
				}
			} catch (OperationCanceledException e) {
				// thrown when cancelling the AST creation
			}
			return Status.CANCEL_STATUS;
		}
	}
	
		
	private Map fListenerGroups;
	
	private SelectionListenerWithSMManager() {
		fListenerGroups= new HashMap();
	}
	
	/**
	 * Registers a selection listener for the given editor part.
	 * @param part The editor part to listen to.
	 * @param listener The listener to register.
	 */
	public void addListener(ITextEditor part, ISelectionListenerWithSM listener) {
		PartListenerGroup partListener= (PartListenerGroup) fListenerGroups.get(part);
		if (partListener == null) {
			partListener= new PartListenerGroup(part);
			fListenerGroups.put(part, partListener);
		}
		partListener.install(listener);
	}

	/**
	 * Unregisters a selection listener.
	 * @param part The editor part the listener was registered.
	 * @param listener The listener to unregister.
	 */
	public void removeListener(ITextEditor part, ISelectionListenerWithSM listener) {
		PartListenerGroup partListener= (PartListenerGroup) fListenerGroups.get(part);
		if (partListener != null) {
			partListener.uninstall(listener);
			if (partListener.isEmpty()) {
				fListenerGroups.remove(part);
			}
		}
	}
	
	/**
	 * Forces a selection changed event that is sent to all listeners registered to the given editor
	 * part. The event is sent from a background thread: this method call can return before the listeners
	 * are informed.
	 */
	public void forceSelectionChange(ITextEditor part, ITextSelection selection) {
		PartListenerGroup partListener= (PartListenerGroup) fListenerGroups.get(part);
		if (partListener != null) {
			partListener.firePostSelectionChanged(selection);
		}
	}}
