/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.runtime.core.internal.RuntimeExtensionManager;
import org.jboss.tools.runtime.core.model.IRuntimeDetectionResolution;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimeDetectionProblem;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;

public class RuntimeCheckboxTreeViewer extends CheckboxTreeViewer {
	// content assist
	private Shell popupShell;
	private Table popupTable;
	
	public RuntimeCheckboxTreeViewer(Composite parent, final Set<RuntimePath> runtimePaths2, int heightHint) {
		this(parent, runtimePaths2.toArray(new RuntimePath[runtimePaths2.size()]), heightHint);
	}
	
	public RuntimeCheckboxTreeViewer(Composite parent, final RuntimePath[] runtimePaths2, int heightHint) {
		super(parent, SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		
		GridData gd;
		Tree tree = getTree();
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		GC gc = new GC( parent);
		FontMetrics fontMetrics = gc.getFontMetrics( );
		gc.dispose( );
		gd.minimumHeight = Dialog.convertHeightInCharsToPixels(fontMetrics, heightHint);
		tree.setLayoutData(gd);
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);

		String[] columnNames = new String[] { Messages.RuntimeCheckboxTreeViewer_Name, 
				Messages.RuntimeCheckboxTreeViewer_Type, Messages.RuntimeCheckboxTreeViewer_Version, 
				Messages.RuntimeCheckboxTreeViewer_Errors, Messages.RuntimeCheckboxTreeViewer_Location};
		int[] columnWidths = new int[] {200, 70, 60, 150, 150};
		
		for (int i = 0; i < columnNames.length; i++) {
			TreeViewerColumn tc = new TreeViewerColumn(this, SWT.NONE);
			tc.getColumn().setText(columnNames[i]);
			tc.getColumn().setWidth(columnWidths[i]);
		}

		setLabelProvider(new RuntimeLabelProvider());
		List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		for (RuntimePath runtimePath:runtimePaths2) {
			runtimeDefinitions.addAll(Arrays.asList(runtimePath.getRuntimeDefinitions()));
		}
		setContentProvider(new RuntimeContentProvider(runtimeDefinitions));
		setInput(runtimeDefinitions);
		for (RuntimeDefinition definition:runtimeDefinitions) {
			setChecked(definition, definition.isEnabled());
		}
		
		addContentAssist();
	}
	
	private void addContentAssist() {

		// content assist
		popupShell = new Shell(getTree().getShell(), SWT.ON_TOP | SWT.RESIZE);
		popupShell.setLayout(new FillLayout());
		popupTable = new Table(popupShell, SWT.SINGLE);
		popupShell.setVisible(false);
		getTree().addMouseListener(new MouseAdapter() {
	        public void mouseDown(MouseEvent event) {
	        	if( popupShell.getVisible()) {
	        		popupShell.setVisible(false);
	        	} else {
	        		showContentAssist(event);
	        	}
	        }
	      });
		

		getTree().addListener(SWT.KeyDown, new Listener() {
			public void handleEvent(Event event) {
				switch (event.keyCode) {
				case SWT.ARROW_DOWN:
					int index = (popupTable.getSelectionIndex() + 1) % popupTable.getItemCount();
					popupTable.setSelection(index);
					event.doit = false;
					break;
				case SWT.ARROW_UP:
					index = popupTable.getSelectionIndex() - 1;
					if (index < 0) index = popupTable.getItemCount() - 1;
					popupTable.setSelection(index);
					event.doit = false;
					break;
				case SWT.CR:
					if (popupShell.isVisible() && popupTable.getSelectionIndex() != -1) {
						event.doit = false;
						contentAssistChosen();
					}
					break;
				case SWT.ESC:
					popupShell.setVisible(false);
					break;
			}
			}
		}); 
		getTree().getShell().addListener(SWT.KeyDown, new Listener() {
			public void handleEvent(Event event) {
				if( popupShell.isVisible() ) {
					if( event.keyCode == SWT.CR || event.keyCode == SWT.ESC) {
						// pre-empt the wizard from closing
						event.doit = false;
					}
				}
			}
		});
		getTree().getShell().addListener(SWT.Traverse, new Listener() {
			public void handleEvent(Event event) {
				if( popupShell.isVisible() ) {
					if (event.detail == SWT.TRAVERSE_ESCAPE) {
						// pre-empt the wizard from closing
						event.doit = false;
					}
				}
			}
			
		});

		Listener hideUnfocusedPopup = new Listener() {
			public void handleEvent(final Event event) {
				Display.getDefault().asyncExec(new Runnable() {
					public void run() {
						Widget w = event.widget;
						Control control = Display.getDefault().getFocusControl();
						if (control == null || (control != getTree() && control != popupTable)) {
							popupShell.setVisible(false);
						}
					}
				});
			}
		};
		popupTable.addListener(SWT.KeyDown, new Listener() {
			public void handleEvent(Event event) {
				if (event.keyCode == SWT.ESC) {
					popupShell.setVisible(false);
				}
			}
		});
		//getTree().addListener(SWT.FocusOut, hideUnfocusedPopup);
		popupTable.addListener(SWT.FocusOut, hideUnfocusedPopup);
		
		Listener hidePopup = new Listener() {
			public void handleEvent(Event event) {
				popupShell.setVisible(false);
			}
		};
		getTree().getShell().addListener(SWT.Dispose, hidePopup);
		getTree().getShell().addListener(SWT.Move, hidePopup);
		
		popupTable.addMouseListener(new MouseAdapter() {

			@Override
			public void mouseDoubleClick(MouseEvent e) {
				contentAssistChosen();
			}
		});
	}
	
	private void contentAssistChosen() {
		int problemIndex = popupTable.getSelectionIndex();
		if( problemIndex != -1 ) {
			RuntimeDefinition sel = getSelectedDefinition();
			RuntimeDetectionProblem[] s = sel.getProblems();
			if( problemIndex < s.length) {
				// TODO  run the content assist action
				// We now have the runtime definition and the chosen problem to be fixed
				final RuntimeDetectionProblem problem = s[problemIndex];
				IRuntimeDetectionResolution[] resolutions = 
						RuntimeExtensionManager.getDefault().findResolutions(problem, sel);
				if( resolutions.length > 0 ) {
					// For now just execute the first one...  TODO expand
					resolutions[0].run(problem, sel);
					
					// The solution may have fixed several problems, not just one
					Object[] all = ((ITreeContentProvider)getContentProvider()).getElements(null);
					for( int i = 0; i < all.length; i++ ) {
						if( all[i] instanceof RuntimeDefinition ) {
							((RuntimeDefinition)all[i]).refreshProblems();
						}
					}
					refresh();
				} else {
					// Show an error that no resolution could be found
					popupShell.setVisible(false);
					StatusDialog d = new StatusDialog(getTree().getShell()) {
						protected Control createDialogArea(Composite parent) {
							Composite composite= (Composite) super.createDialogArea(parent);

							Label label= new Label(composite, SWT.WRAP);
							GridData layoutData= new GridData(SWT.FILL, SWT.CENTER, true, false);
							layoutData.widthHint= convertWidthInCharsToPixels(80);
							label.setLayoutData(layoutData);
							label.setText("No quickfixes were found for the problem: " + problem.getDescription());
							updateStatus(new Status(IStatus.ERROR, RuntimeUIActivator.PLUGIN_ID, "No quickfixes found for the selected problem"));
							Dialog.applyDialogFont(composite);
							return composite;
						}
					};
					d.setTitle("No quickfix available");
					d.open();
				}
			}
		}
		popupShell.setVisible(false);
	}
	
	private void showContentAssist(MouseEvent event) {
		Rectangle itemBounds = findItemBounds(event, 3);
		
		if( itemBounds != null ) {
			popupTable.removeAll();
			RuntimeDefinition selected = getSelectedDefinition();
			if( selected != null ) {
				RuntimeDetectionProblem[] s = selected.getProblems();
				for( int i = 0; i < s.length; i++ ) {
					String text = s[i].getLabel();
					if( text != null ) {
						int sev = s[i].getSeverity();
						TableItem ti = new TableItem(popupTable, SWT.NONE);
						ti.setText(text);
						Image sevImage = null;
						if( sev == IStatus.ERROR ) {
							sevImage = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_DEC_FIELD_ERROR);
						} else if( sev == IStatus.WARNING) {
							sevImage = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_DEC_FIELD_WARNING);
						}
						if( sevImage != null ) 
							ti.setImage(sevImage);
					}
				}
			}
			
			
			String[] text = getQuickfixTextForCurrentSelection();
			// Don't show it if we have no errors
			if( text == null || text.length == 0 ) {
				return;
			}
			for (int i = 0; i < text.length; i++) {
			}
			Rectangle textBounds = Display.getDefault().map(getTree(), null, itemBounds);
			popupShell.setBounds(textBounds.x, textBounds.y + (textBounds.height), textBounds.width > 250 ? textBounds.width : 250, 150);
			popupTable.setEnabled(true);
			popupShell.setVisible(true);
		}
	}
	
	private String[] getQuickfixTextForCurrentSelection() {
		// TODO Delete this
		RuntimeDefinition selected = getSelectedDefinition();
		if( selected != null ) {
			RuntimeDetectionProblem[] s = selected.getProblems();
			ArrayList<String> ret = new ArrayList<String>();
			for( int i = 0; i < s.length; i++ ) {
				ret.add(s[i].getLabel());
			}
			return (String[]) ret.toArray(new String[ret.size()]);
		}
		return new String[]{};
	}
	
	private RuntimeDefinition getSelectedDefinition() {
		ISelection sel = getSelection();
		if( sel instanceof IStructuredSelection ) {
			Object f = ((IStructuredSelection)sel).getFirstElement();
			if( f instanceof RuntimeDefinition ) {
				return ((RuntimeDefinition)f);
			}
		}
		return null;
	}
	
	private Rectangle findItemBounds(MouseEvent event, int soughtColumn) {
		Point pt = new Point(event.x, event.y);

		TreeItem item = getTree().getItem(pt);
		if (item != null) {
			for (int i = 0, n = getTree().getColumnCount(); i < n; i++) {
				Rectangle rect = item.getBounds(i);
				if (rect.contains(pt)) {
					if (i == soughtColumn) {
						return rect;
					}
				}
			}
		}
		return null;
	}
	
	
	// Refresh your input given the following RuntimePath[]
	public void updateInput(RuntimePath[] runtimePaths) {
		setInput(null);
		List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		for (RuntimePath runtimePath : runtimePaths) {
			runtimeDefinitions.addAll(Arrays.asList(runtimePath.getRuntimeDefinitions()));
		}
		setInput(runtimeDefinitions);
		for (RuntimeDefinition runtimeDefinition : runtimeDefinitions) {
			setChecked(runtimeDefinition,runtimeDefinition.isEnabled());
		}
	}
}
