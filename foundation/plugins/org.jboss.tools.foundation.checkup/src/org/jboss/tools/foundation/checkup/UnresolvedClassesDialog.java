/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.checkup;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PatternFilter;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.UnresolvedClass;

public class UnresolvedClassesDialog extends Dialog{
	public static volatile boolean showing = false;
	private List<UnresolvedClass> classes;
	private String currnetJavaVersion = "";
	private boolean showNextTime = true;
	
	public UnresolvedClassesDialog(Shell parentShell, List<UnresolvedClass> classes, String currentJavaVersion) {
		super(parentShell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		this.classes = classes;
		this.currnetJavaVersion = currentJavaVersion;
	}
	
	public boolean showNextTime(){
		return showNextTime;
	}

	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
	}

	protected Control createDialogArea(Composite parent) {
		getShell().setText("Warning");

		Composite title = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginTop = layout.marginBottom = layout.marginLeft = layout.marginRight = 5;
		title.setLayout(layout);
		GridData data = new GridData(SWT.LEFT, SWT.FILL, true, false);
		data.widthHint = 500;
		title.setLayoutData(data);
		
		Composite composite = new Composite(parent, SWT.NONE);
		layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginTop = layout.marginBottom = layout.marginLeft = layout.marginRight = 5;
		composite.setLayout(layout);
		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		
		composite.setLayoutData(data);
		
		// create image
	    Image image = PlatformUI.getWorkbench().getDisplay().getSystemImage(SWT.ICON_WARNING);
	    Label warning = new Label(title, 0);
	    image.setBackground(warning.getBackground());
	    warning.setImage(image);
	    warning.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING| GridData.VERTICAL_ALIGN_CENTER));
	    
		// message
		Label label = new Label(title, SWT.WRAP);
		data = new GridData(SWT.LEFT, SWT.FILL, true, false);
		label.setLayoutData(data);
		label.setFont(parent.getFont());
		StringBuilder sb = new StringBuilder();
		
		sb.append(NLS.bind(JVMProblemDetectorMessages.UNRESOLVED_CLASSES_WARNING_DIALOG_MESSAGE, currnetJavaVersion));
		
		label.setText(sb.toString());
		
		final ListViewer list = new ListViewer(composite, SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER);
		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		data.heightHint = 200;

		list.getList().setLayoutData(data);
		list.setContentProvider(new ListContent());
		list.setLabelProvider(new ListLabelProvider());
		list.setInput(classes);
		
		label = new Label(composite, SWT.WRAP);
		data = new GridData(SWT.LEFT, SWT.FILL, true, false);
		label.setLayoutData(data);
		label.setFont(parent.getFont());
		label.setText(JVMProblemDetectorMessages.UNRESOLVED_CLASSES_WARNING_DIALOG_ADVISE);
		
		final Button button = new Button(composite, SWT.CHECK);
		button.setText(JVMProblemDetectorMessages.DO_NOT_SHOW);
		button.addSelectionListener(new SelectionListener(){
			public void widgetSelected(SelectionEvent e) {
				showNextTime = !button.getSelection();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
			}
			
		});

		return composite;
	}
	
	public class ListContent implements IStructuredContentProvider {

		public Object[] getElements(Object inputElement) {
			if(inputElement == classes){
				return classes.toArray();
			}
			return null;
		}

		public void dispose() {
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
	}
	
	public class ListLabelProvider implements ILabelProvider{

		public void addListener(ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		public void removeListener(ILabelProviderListener listener) {
		}

		public Image getImage(Object element) {
			return null;
		}

		public String getText(Object element) {
			if(element instanceof UnresolvedClass){
				return ((UnresolvedClass) element).toString();
			}
			return null;
		}
		
	}
	
	public boolean close() {
		showing = false;
		return super.close();
	}

	public int open() {
		showing = true;
		return super.open();
	}
}
