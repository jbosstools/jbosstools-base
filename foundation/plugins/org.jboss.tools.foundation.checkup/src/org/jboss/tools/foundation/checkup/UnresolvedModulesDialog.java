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
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PatternFilter;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.Dependant;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.DependantList;
import org.jboss.tools.foundation.checkup.JVMProblemDetector.UnresolvedModule;

public class UnresolvedModulesDialog extends Dialog{
	public static volatile boolean showing = false;
	private List<UnresolvedModule> modules;
	private String currnetJavaVersion = "";
	private boolean showNextTime = true;
	private ModuleFilter patternFilter;
	
	public UnresolvedModulesDialog(Shell parentShell, List<UnresolvedModule> modules, String currentJavaVersion) {
		super(parentShell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		this.modules = modules;
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
		
		sb.append(NLS.bind(JVMProblemDetectorMessages.UNRESOLVED_MODULES_WARNING_DIALOG_MESSAGE, currnetJavaVersion));
		
		label.setText(sb.toString());
		
		final Text fFilterText= new Text(composite, SWT.SEARCH|SWT.ICON_SEARCH);
		data = new GridData(SWT.FILL, SWT.BEGINNING, true, false);
		fFilterText.setLayoutData(data);
		Dialog.applyDialogFont(fFilterText);
		
		final TreeViewer tree = new TreeViewer(composite, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		tree.setAutoExpandLevel(TreeViewer.ALL_LEVELS);
		data = new GridData(SWT.FILL, SWT.FILL, true, true);
		data.heightHint = 200;

		tree.getTree().setLayoutData(data);
		tree.setContentProvider(new TreeContent());
		tree.setLabelProvider(new LabelProvider());
		tree.setInput(modules);
		patternFilter = new ModuleFilter();
		tree.setFilters(new ViewerFilter[]{patternFilter});
		
		fFilterText.setText(""); //$NON-NLS-1$

		fFilterText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				patternFilter.setPattern(fFilterText.getText());
				tree.refresh();
				tree.expandAll();
			}
		});
		
		label = new Label(composite, SWT.WRAP);
		data = new GridData(SWT.LEFT, SWT.FILL, true, false);
		label.setLayoutData(data);
		label.setFont(parent.getFont());
		label.setText(JVMProblemDetectorMessages.UNRESOLVED_MODULES_WARNING_DIALOG_ADVISE);
		
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
	
	class ModuleFilter extends PatternFilter{
		protected boolean isLeafMatch(Viewer viewer, Object element){
	        String labelText = ((ILabelProvider) ((StructuredViewer) viewer)
	                .getLabelProvider()).getText(element);
	        
	        if(labelText == null) {
				return false;
			}
	        boolean result = wordMatches(labelText);
	        if(!result){
	        	labelText = labelText.replace('.', ' ');
	        	return wordMatches(labelText);
	        }
	        return true;
	    }
	}
	
	public class TreeContent implements ITreeContentProvider {

		public void dispose() {
		}
	
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
	
		public Object[] getElements(Object inputElement) {
			if(inputElement == modules){
				return modules.toArray();
			}else if(inputElement instanceof UnresolvedModule){
				return new DependantList[]{((UnresolvedModule)inputElement).getDependantList()};
			}else if(inputElement instanceof DependantList){
				return ((DependantList) inputElement).getDependants().toArray();
			}
			return null;
		}
	
		public Object[] getChildren(Object parentElement) {
			if(parentElement == modules){
				return modules.toArray();
			}else if(parentElement instanceof UnresolvedModule){
				return new DependantList[]{((UnresolvedModule)parentElement).getDependantList()};
			}else if(parentElement instanceof DependantList){
				return ((DependantList) parentElement).getDependants().toArray();
			}
			return null;
		}
	
		public Object getParent(Object element) {
			if(element instanceof Dependant){
				return ((Dependant) element).getParent().getDependantList();
			}else if(element instanceof UnresolvedModule){
				return modules;
			}else if(element instanceof DependantList){
				((DependantList) element).getUnresolvedModule();
			}
			return null;
		}
	
		public boolean hasChildren(Object inputElement) {
			if(inputElement == modules){
				return modules.size() > 0; 
			}else if(inputElement instanceof UnresolvedModule){
				return ((UnresolvedModule) inputElement).getDependantList().getDependants().size() > 0;
			}else if(inputElement instanceof DependantList){
				return ((DependantList) inputElement).getDependants().size() > 0;
			}
			return false;
		}

	}
	
	public class LabelProvider implements ILabelProvider{

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
			if(element instanceof Dependant){
				return ((Dependant) element).toString();
			}else if(element instanceof UnresolvedModule){
				return ((UnresolvedModule) element).toString();
			}else if(element instanceof DependantList){
				return JVMProblemDetectorMessages.DEPENDANT_MODULES;
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
