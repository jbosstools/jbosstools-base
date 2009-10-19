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
package org.jboss.tools.common.model.ui.editors.dnd.composite;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnPixelData;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.jboss.tools.common.model.ui.editors.dnd.DropData;
import org.jboss.tools.common.model.ui.editors.dnd.IDropWizardModel;
import org.jboss.tools.common.model.ui.editors.dnd.ITagProposal;
import org.jboss.tools.common.model.ui.editors.dnd.ITagProposalFactory;

public class TagProposalsComposite extends Composite {
	
	private ITagProposal selection = IDropWizardModel.UNDEFINED_TAG_PROPOSAL;
	CheckboxTableViewer tableTreeViewer;
	ITagProposal[] fTagProposals;
	IDropWizardModel fModel; 	
	
	public TagProposalsComposite(Composite parent, int styles,IDropWizardModel model) {
		super(parent,styles);
		fModel = model;
		fTagProposals = 
			model.getTagProposalFactory().getProposalLoader(
					fModel.getDropData().getMimeType()
				).getTagProposals(
					fModel.getDropData()
				);
		
		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.numColumns = 1;
		setLayout(layout);

		GridData data = new GridData(GridData.FILL_BOTH);
		setLayoutData(data);
	
	
	    Table swtTable = new Table(this, SWT.BORDER | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE | SWT.CHECK);
		data = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
    
		TableLayout tableLayout = new TableLayout();

	    tableTreeViewer =
		new CheckboxTableViewer(swtTable);

	    swtTable.setLayout(tableLayout);
	    swtTable.setLayoutData(data);
	    swtTable.setHeaderVisible(true);
	    swtTable.setLinesVisible(true);

		tableLayout.addColumnData(new ColumnPixelData(150));
		TableColumn col = new TableColumn(swtTable, SWT.NONE);
		col.setText("Tag name");
		
		tableLayout.addColumnData(new ColumnPixelData(250));
		col = new TableColumn(swtTable, SWT.NONE);
		col.setText("URI");

		tableTreeViewer.setColumnProperties(new String[] {"Tag Name", "URI"});      
		
		tableTreeViewer.addCheckStateListener(
			new ICheckStateListener() {
				public void checkStateChanged(CheckStateChangedEvent event) {
					ITagProposal proposal = (ITagProposal)event.getElement();
					if (event.getChecked()) {
						if(selection!=IDropWizardModel.UNDEFINED_TAG_PROPOSAL) {
							tableTreeViewer.setChecked(selection, false);								
						}
						selection = proposal;
					} else {
						selection = IDropWizardModel.UNDEFINED_TAG_PROPOSAL;
					}
					fModel.setTagProposal(selection);					
				}
			}
		);		
	
		tableTreeViewer.setContentProvider(
			new IStructuredContentProvider() {

				public Object[] getElements(Object inputElement) {
						return fTagProposals;
				}

				public void dispose() {
				}

				public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
				}
			}
		);
		
		tableTreeViewer.setLabelProvider(
			new ITableLabelProvider () {
				
				public static final int TAG_NAME_INDEX = 0;
				public static final int TAG_URI_INDEX = 1;
				
				public Image getColumnImage(Object element, int columnIndex) {
					return null;
				}
	
				public String getColumnText(Object element, int columnIndex) {
					ITagProposal prop = (ITagProposal)element;
					switch (columnIndex) {
						case TAG_NAME_INDEX:
							return prop.getDisplayString();
							
						case TAG_URI_INDEX:
							return prop.getDetails();
					}
					throw new IllegalArgumentException("Wrong column index for LabelProvider"); //$NON-NLS-1$
				}
	
				public void addListener(ILabelProviderListener listener) {
				}
	
				public void dispose() {
				}
	
				public boolean isLabelProperty(Object element, String property) {
					return false;
				}
	
				public void removeListener(ILabelProviderListener listener) {
				}
			}
		);
		tableTreeViewer.setInput("ROOT");
	}
	
///	public void setTagProposalsSelectionListener(ITagProposalSelectionListener l) {
///		listener = l;
///	}
	
	public ITagProposal[] getTagProposals() {
		return fTagProposals;
	}
	
	public boolean hasTagProposals() {
		return getTagProposals().length>0;
	}
	
	public static boolean areThereTagProposals(String mimeType,DropData dropData, ITagProposalFactory tagProposalFactory) {
		return tagProposalFactory.getProposalLoader(mimeType).getTagProposals(dropData).length>0;		
	}
	
	public static ITagProposal[] getTagProposals(String mimeType,DropData dropData, ITagProposalFactory tagProposalFactory) {
		return tagProposalFactory.getProposalLoader(mimeType).getTagProposals(dropData);		
	}
	
}
