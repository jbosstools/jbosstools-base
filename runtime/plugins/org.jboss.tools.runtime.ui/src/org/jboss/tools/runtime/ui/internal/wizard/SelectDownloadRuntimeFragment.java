/*************************************************************************************
 * Copyright (c) 2010-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Table;
import org.jboss.tools.foundation.ui.util.BrowserUtility;
import org.jboss.tools.foundation.ui.xpl.taskwizard.IWizardHandle;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.RuntimeUIExtensionManager;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.dialogs.AutoResizeTableLayout;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class SelectDownloadRuntimeFragment extends WizardFragment {
	private Map<String, DownloadRuntime> downloadRuntimes;
	private TableViewer viewer;
	private Link urlLink, projectLink; 
	private Label restrictionsLabel;
	
	private DownloadRuntime selectedRuntime;
	private String projectLinkText = null;
	private String urlLinkText = null;
	private String restrictionsLabelText = null;
	
	private IWizardHandle handle;
	
	/**
	 * This method is unsafe because it provides the opportunity to block during the call to getDownloadRuntimes() 
	 * @deprecated
	 */
	public SelectDownloadRuntimeFragment() {
		this(RuntimeCoreActivator.getDefault().getDownloadRuntimes());
	}
	public SelectDownloadRuntimeFragment(Map<String, DownloadRuntime> downloadRuntimes) {
		this.downloadRuntimes = downloadRuntimes;
	}
	
	@Override
	public boolean hasComposite() {
		return true;
	}


	@Override
	public Composite createComposite(final Composite parent, IWizardHandle handle) {
		this.handle = handle;
		getPage().setTitle(Messages.DownloadRuntimesWizardFirstPage_Download_Runtimes);
		getPage().setDescription(Messages.DownloadRuntimesWizardFirstPage_Please_select_a_runtime);
		
		Composite contentsWrapper = new Composite(parent, SWT.BORDER);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		contentsWrapper.setLayoutData(gd); 
		contentsWrapper.setLayout(new FormLayout());
		
		SashForm contents = new SashForm(contentsWrapper, SWT.VERTICAL);
		FormData contentsData = new FormData();
		contentsData.top = new FormAttachment(0,0);
		contentsData.bottom = new FormAttachment(100,0);
		contentsData.left = new FormAttachment(0,0);
		contentsData.right = new FormAttachment(100,0);
		contentsData.width = 200;
		contentsData.height = 400;
		contents.setLayoutData(contentsData);
		
		if (downloadRuntimes == null || downloadRuntimes.isEmpty()) {
			new Label(contents, SWT.NONE).setText(Messages.DownloadRuntimesWizardFirstPage_No_available_runtime);
			setComplete(false);
			return contentsWrapper;
		}
		
		viewer = createTable(contents, parent.getFont());
		
		final Group g = new Group(contents, SWT.DEFAULT);
		g.setText("Selected Runtime Details");
		g.setLayout(new GridLayout(2, false));
		
		Label pUrlLabel = new Label(g, SWT.NONE); 
		pUrlLabel.setText("Project URL: ");
		projectLink = new Link(g, SWT.NONE);
		Label dUrlLabel = new Label(g, SWT.NONE); 
		dUrlLabel.setText("Download URL: ");
		urlLink = new Link(g, SWT.NONE);
		GridData plGD = new GridData(SWT.FILL, SWT.FILL, true, false);
		GridData dlGD = new GridData(SWT.FILL, SWT.FILL, true, true);
		GridData lGD = new GridData(SWT.FILL, SWT.FILL, false, false);
		pUrlLabel.setLayoutData(lGD);
		dUrlLabel.setLayoutData(lGD);
		urlLink.setLayoutData(dlGD);
		projectLink.setLayoutData(plGD);
		
		
		restrictionsLabel = new Label(g, SWT.WRAP);
		GridData restrictionLabelGD = new GridData(SWT.FILL, SWT.FILL, true, true);
		restrictionLabelGD.horizontalSpan = 2;
		restrictionsLabel.setLayoutData(restrictionLabelGD);
		
		urlLink.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				new BrowserUtility().checkedCreateExternalBrowser(selectedRuntime.getUrl(),
						RuntimeUIActivator.PLUGIN_ID, RuntimeUIActivator.getDefault().getLog());
			}
		});
		projectLink.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				new BrowserUtility().checkedCreateExternalBrowser(selectedRuntime.getHumanUrl(),
						RuntimeUIActivator.PLUGIN_ID, RuntimeUIActivator.getDefault().getLog());
			}
		});
		
		contents.setWeights(new int[]{5,2});

		StructuredSelection ss = new StructuredSelection();
		viewer.setSelection(ss);
		return contentsWrapper;
	}
	

	protected TableViewer createTable(Composite contents, Font font) {
		TableViewer viewer = new TableViewer(contents, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.BORDER);
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		table.setFont(font);
		
		viewer.setContentProvider(new DownloadRuntimesContentProvider());
		
		String[] columnHeaders = {Messages.DownloadRuntimesWizardFirstPage_Name, 
				Messages.DownloadRuntimesWizardFirstPage_Version};
		
		for (int i = 0; i < columnHeaders.length; i++) {
			TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
			column.setLabelProvider(new DownloadRuntimesLabelProvider(i));
			column.getColumn().setText(columnHeaders[i]);
			column.getColumn().setResizable(true);
			column.getColumn().setMoveable(true);
		}
		
		ColumnLayoutData[] runtimePathsLayouts= {
				new ColumnWeightData(100,100),
				new ColumnWeightData(50,50),
			};
		
		TableLayout layout = new AutoResizeTableLayout(table);
		for (int i = 0; i < runtimePathsLayouts.length; i++) {
			layout.addColumnData(runtimePathsLayouts[i]);
		}
		
		viewer.setInput(downloadRuntimes);
		viewer.getTable().setLayout(layout);
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection sel = event.getSelection();
				selectRuntime(sel);
			}
		});
		
		return viewer;
	}
	
	
	private HashMap<DownloadRuntime, WizardFragment[]> fragmentMap;
	public List<WizardFragment> getChildFragments() {
		ArrayList<WizardFragment> list = new ArrayList<WizardFragment>();
		if( selectedRuntime != null ) {
			if( fragmentMap == null ) {
				fragmentMap = new HashMap<DownloadRuntime, WizardFragment[]>();
			}
			WizardFragment[] frags = fragmentMap.get(selectedRuntime);
			if( frags == null ) {
				frags = RuntimeUIExtensionManager.createFragmentsForRuntime(selectedRuntime);
				fragmentMap.put(selectedRuntime, frags);
			}
			WizardFragment[] nullSafe = (frags == null ? new WizardFragment[0] : frags);
			list.addAll(Arrays.asList(nullSafe));
		}
		return list;
	}
	
	
	private void selectRuntime(ISelection sel, boolean updateWidgets) {
		selectedRuntime = null;
		setComplete(false);
		if (sel instanceof IStructuredSelection) {
			selectedRuntime = (DownloadRuntime) ((IStructuredSelection) sel).getFirstElement();
			if( selectedRuntime != null ) {
				String projectUrl = selectedRuntime.getHumanUrl();
				String dlUrl = selectedRuntime.getUrl();
				if( projectLink != null ) {
					projectLinkText = projectUrl == null ? "None" : "<a>" + projectUrl + "</a>";
				}
				if( urlLink != null ) {
					urlLinkText = dlUrl == null ? "None" : "<a>" + dlUrl + "</a>";
				}
				if( restrictionsLabel != null ) {
					Object subscription = selectedRuntime.getProperty(DownloadRuntime.PROPERTY_REQUIRES_CREDENTIALS);
					if( subscription != null && Boolean.parseBoolean(subscription.toString())) {
						restrictionsLabelText = "Registration required. Downloads require accepting the terms and conditions of the JBoss Developer Program which provides $0 subscriptions for development use only.";
					} else {
						restrictionsLabelText = "";
					}
				}
			}
		}
		setComplete(selectedRuntime != null);
		getTaskModel().putObject(DownloadRuntimesTaskWizard.DL_RUNTIME_PROP, selectedRuntime);
		handle.update();
		
		if( updateWidgets) {
			projectLink.setText(projectLinkText == null ? "" : projectLinkText);
			urlLink.setText(urlLinkText == null ? "" : urlLinkText);
			restrictionsLabel.setText(restrictionsLabelText == null ? "" : restrictionsLabelText);
		}
	}

	private void selectRuntime(ISelection sel) {
		selectRuntime(sel, true);	
	}

	class DownloadRuntimesContentProvider implements IStructuredContentProvider {

		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			
		}
		
		private Comparator<DownloadRuntime> getDownloadRuntimeComparator() {
			final String VERSION_REGEX_STRING = "([^\\d]*)(\\d+)\\.(\\d+)\\.(\\d+)(.*)";
			final Pattern VERSION_PATTERN = Pattern.compile(VERSION_REGEX_STRING);

			return new Comparator<DownloadRuntime>() {
				@Override
				public int compare(DownloadRuntime o1, DownloadRuntime o2) {
					String n1 = o1.getName().trim();
					String n2 = o2.getName().trim();
					Matcher m1 = VERSION_PATTERN.matcher(n1);
					Matcher m2 = VERSION_PATTERN.matcher(n2);
					if( m1.matches() && m2.matches() ) {
						String prefix1 = m1.group(1);
						String prefix2 = m2.group(1);
						if( prefix1.equals(prefix2)) {
							for( int i = 2; i <= 4; i++ ) {
								int segmentA = Integer.parseInt(m1.group(i));
								int segmentB = Integer.parseInt(m2.group(i));
								if( segmentA != segmentB ) {
									int ret = segmentA - segmentB;
									return ret;
								}
							}
							String suffix1 = m1.group(5);
							String suffix2 = m2.group(5);
							int ret = suffix1.compareTo(suffix2);
							return ret;
						}
						
					}
					int ret = n1.compareTo(n2);
					return ret;
				}
				
			};
		}
		
		@Override
		public Object[] getElements(Object inputElement) {
			DownloadRuntime[] runtimes = downloadRuntimes.values().toArray(new DownloadRuntime[0]);
			Arrays.sort(runtimes, getDownloadRuntimeComparator());
			return runtimes;
		}
		
		@Override
		public void dispose() {
			
		}
	}
	
	class DownloadRuntimesLabelProvider extends ColumnLabelProvider {

		private int columnIndex;

		public DownloadRuntimesLabelProvider(int i) {
			this.columnIndex = i;
		}

		public String getText(Object element) {
			if (element instanceof DownloadRuntime) {
				DownloadRuntime downloadRuntime = (DownloadRuntime) element;
				switch (columnIndex) {
				case 0:
					return downloadRuntime.getName();
				case 1:
					return downloadRuntime.getVersion();
				case 2:
					return downloadRuntime.getUrl();
				}
			}
			return null;
		}
	}

}
