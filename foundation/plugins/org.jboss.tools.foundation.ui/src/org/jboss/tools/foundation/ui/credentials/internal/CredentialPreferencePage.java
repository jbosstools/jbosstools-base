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
package org.jboss.tools.foundation.ui.credentials.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.foundation.core.credentials.CredentialService;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;
import org.jboss.tools.foundation.ui.credentials.internal.FaviconCache.FaviconCacheListener;
import org.jboss.tools.foundation.ui.internal.FoundationUIPlugin;
import org.jboss.tools.foundation.ui.util.FormDataUtility;

public class CredentialPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
	private Button addDomainButton;
	private Button removeDomainButton;
	private Button addUserButton;
	private Button removeUserButton;
	private Button editButton;  
	private TreeViewer tv;
	
	
	public CredentialPreferencePage() {
		noDefaultAndApplyButton();
	}

	public CredentialPreferencePage(String title) {
		super(title);
	}

	public CredentialPreferencePage(String title, ImageDescriptor image) {
		super(title, image);
	}

	@Override
	public void init(IWorkbench workbench) {
	}

	
	private static class CredentialTreeContentProvider implements ITreeContentProvider {
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
		public void dispose() {
		}
		public boolean hasChildren(Object element) {
			return getChildren(element).length > 0;
		}
		public Object getParent(Object element) {
			return null;
		}
		public Object[] getElements(Object inputElement) {
			return CredentialService.getCredentialModel().getDomains();
		}
		public Object[] getChildren(Object parentElement) {
			if( parentElement instanceof ICredentialDomain) {
				ICredentialDomain p = (ICredentialDomain)parentElement;
				String[] all = ((ICredentialDomain)parentElement).getUsernames();
				ArrayList<CredentialUser> ret = new ArrayList<CredentialUser>(all.length);
				for( int i = 0; i < all.length; i++ ) {
					ret.add(new CredentialUser(p, all[i]));
				}
				
				// Sort the users to be alphabetical, but with default on top
				Collections.sort(ret, getCredentialUserComparator());
				return (CredentialUser[]) ret.toArray(new CredentialUser[ret.size()]);
			}
			return new Object[0];
		}
	}
	
	private static class CredentialUserComparator implements Comparator<CredentialUser> {
		public int compare(CredentialUser o1, CredentialUser o2) {
			int ret = 0;
			String defaultUser = o1.domain.getDefaultUsername();
			if( defaultUser != null ) {
				if( o1.user.equals(defaultUser)) {
					ret = -1;
				} else if( o2.user.equals(defaultUser)) {
					ret = 1;
				} else {
					ret = o1.user.compareTo(o2.user);
				}
			}
			return ret;
		}
	}
	private static Comparator<CredentialUser> getCredentialUserComparator() {
		return new CredentialUserComparator();
	}
	
	private ITreeContentProvider createContentProvider() {
		return new CredentialTreeContentProvider();
	}
	
	/**
	 * A representation of a domain / user pair
	 */
	private static class CredentialUser {
		private ICredentialDomain domain;
		private String user;
		public CredentialUser(ICredentialDomain domain, String user) {
			this.domain = domain;
			this.user = user;
		}
	}
	
	private LabelProvider createLabelProvider() {
		return new LabelProvider() {
			public Image getImage(Object element) {
				if( element instanceof CredentialUser) {
					CredentialUser cu = (CredentialUser)element;
					if( cu.user.equals(cu.domain.getDefaultUsername())) {
						return FoundationUIPlugin.getDefault().getSharedImages().image(FoundationUIPlugin.IMAGE_GREEN_CHECK_16);
					}
				}
				if( element instanceof ICredentialDomain) {
					final ICredentialDomain cd = (ICredentialDomain)element;
					String host = cd.getName();
					Image i = FaviconCache.getDefault().getImageForHost(host);
					if( i != null ) 
						return i;
					
					if( !FaviconCache.getDefault().loadFailed(host)) {
						FaviconCacheListener l = new FaviconCacheListener() {
							public void iconCached(String host) {
								Display.getDefault().asyncExec(new Runnable() {
									public void run() {
										tv.refresh();
									}
								});
							}
							public void fetchFailed(String host) {
							}
						};
						
						FaviconCache.getDefault().loadFavicon(host, l);
					}
				}
				return null;
			}
			@Override
			public String getText(Object element) {
				if( element instanceof ICredentialDomain) {
					return ((ICredentialDomain)element).getName();
				}
				if( element instanceof CredentialUser ) {
					return ((CredentialUser)element).user;
				}
				return element == null ? "" : element.toString();//$NON-NLS-1$
			}
		};
	}
	
	@Override
	protected Control createContents(Composite parent) {
		Composite mine = new Composite(parent, SWT.NULL);
		mine.setLayout(new FormLayout());
		
		tv = new TreeViewer(mine, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		tv.setContentProvider(createContentProvider());
		tv.setLabelProvider(createLabelProvider()); 
		tv.setInput(ResourcesPlugin.getWorkspace());
		
		tv.getTree().setLayoutData(new FormDataUtility().createFormData(0, 5, 100, -5, 0, 5, 80, -5));
		
		addDomainButton = new Button(mine, SWT.PUSH);
		removeDomainButton = new Button(mine, SWT.PUSH);
		addUserButton = new Button(mine, SWT.PUSH);
		removeUserButton = new Button(mine, SWT.PUSH);
		editButton = new Button(mine, SWT.PUSH);
		
		addDomainButton.setText(CredentialMessages.AddDomain);
		removeDomainButton.setText(CredentialMessages.RemoveDomain);
		addUserButton.setText(CredentialMessages.AddUser);
		removeUserButton.setText(CredentialMessages.RemoveUser);
		editButton.setText(CredentialMessages.Edit);
		
		addDomainButton.setLayoutData(new FormDataUtility().createFormData(0, 5, null, 0, 80, 5, 100, -5));
		removeDomainButton.setLayoutData(new FormDataUtility().createFormData(addDomainButton, 5, null, 0, 80, 5, 100, -5));
		addUserButton.setLayoutData(new FormDataUtility().createFormData(removeDomainButton, 5, null, 0, 80, 5, 100, -5));
		removeUserButton.setLayoutData(new FormDataUtility().createFormData(addUserButton, 5, null, 0, 80, 5, 100, -5));
		editButton.setLayoutData(new FormDataUtility().createFormData(removeUserButton, 5, null, 0, 80, 5, 100, -5));
		
		removeUserButton.setEnabled(false);
		removeDomainButton.setEnabled(false);
		editButton.setEnabled(false);
		
		tv.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				if( tv.getSelection() instanceof IStructuredSelection ) { 
					IStructuredSelection ss = (IStructuredSelection)tv.getSelection();
					Object selected = ss.getFirstElement();
					if( selected != null ) {
						if( selected instanceof ICredentialDomain) {
							// cannot edit a domain unless you have multiple usersnames. Edit domain just sets the default 
							boolean noUsers = ((ICredentialDomain)selected).getUsernames().length <= 1;
							editButton.setEnabled(!noUsers);
							ICredentialDomain domain = (ICredentialDomain)selected;
							addDomainButton.setEnabled(true);
							addUserButton.setEnabled(true);
							removeDomainButton.setEnabled(domain.getRemovable() && domain.getUsernames().length == 0);
							removeUserButton.setEnabled(false);
						} else if( selected instanceof CredentialUser ) {
							editButton.setEnabled(true);
							addDomainButton.setEnabled(true);
							addUserButton.setEnabled(true);
							removeDomainButton.setEnabled(false);
							removeUserButton.setEnabled(true);
						}
					} else {
						addDomainButton.setEnabled(true);
						addUserButton.setEnabled(true);
						removeDomainButton.setEnabled(false);
						removeUserButton.setEnabled(false);
						editButton.setEnabled(false);
					}
				}
			}
		});
		
		
		addDomainButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				addDomainPressed();
			}
		});
		removeDomainButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				removeDomainPressed();
			}
		});
		addUserButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				addUserPressed();
			}
		});
		removeUserButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				removeUserPressed();
			}
		});
		editButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				editPressed();
			}
		});
		
		return mine;
	}
	
	
	private void addDomainPressed() {
		NewCredentialDomainDialog dialog = new NewCredentialDomainDialog(getShell(), CredentialService.getCredentialModel());
		if( dialog.open() == Window.OK) {
			String name = dialog.getDomainName();
			CredentialService.getCredentialModel().addDomain(name, name, true);
			CredentialService.getCredentialModel().save();
			tv.refresh();
		}
	}
	private void removeDomainPressed() {
		IStructuredSelection ss = (IStructuredSelection)tv.getSelection();
		Object selected = ss.getFirstElement();
		if( selected instanceof ICredentialDomain) {
			ICredentialDomain domain = (ICredentialDomain)selected;
			if( domain.getRemovable() && domain.getUsernames().length == 0) {
				if(!CredentialService.getCredentialModel().save()) {
					//Is not able to save model, because user does not authenticated sequre storage. 
					//Do not complete remove.
					MessageDialog.openWarning(removeUserButton.getShell(), 
							CredentialMessages.Warning, 
							CredentialMessages.UnableToDeleteCredentials);
					return;
				}
				CredentialService.getCredentialModel().removeDomain(domain);
				CredentialService.getCredentialModel().save();
				tv.refresh();
			}
		}
	}
	
	private void editPressed() {
		IStructuredSelection ss = (IStructuredSelection)tv.getSelection();
		Object selected = ss.getFirstElement();
		if( selected instanceof CredentialUser) {
			editUserPressed(((CredentialUser)selected));
		} else if( selected instanceof ICredentialDomain) {
			editDomainPressed(((ICredentialDomain)selected));
		}
	}

	private void editDomainPressed(ICredentialDomain domain) {
		NewCredentialDomainDialog dialog = new NewCredentialDomainDialog(
				getShell(), CredentialService.getCredentialModel(), domain);
		if( dialog.open() == Window.OK) {
			CredentialService.getCredentialModel().setDefaultCredential(domain, dialog.getDefaultUser());
			CredentialService.getCredentialModel().save();
			tv.refresh();
		}

	}
	
	private void editUserPressed(CredentialUser u) {
		NewCredentialUserDialog dialog = new NewCredentialUserDialog(
				getShell(), CredentialService.getCredentialModel(), u.domain, u.user);
		if( dialog.open() == Window.OK) {
			String name = dialog.getUser();
			String pass = dialog.getPass();
			CredentialService.getCredentialModel().removeCredentials(u.domain, u.user);
			if( dialog.isAlwaysPrompt()) {
				CredentialService.getCredentialModel().addPromptedCredentials(u.domain, u.user);
			} else {
				CredentialService.getCredentialModel().addCredentials(u.domain, name, pass);
			}
			CredentialService.getCredentialModel().save();
			tv.refresh();
		}
	}
	
	private void addUserPressed() {
		IStructuredSelection ss = (IStructuredSelection)tv.getSelection();
		Object selected = ss.getFirstElement();
		ICredentialDomain selectedDomain = null;
		if( selected instanceof ICredentialDomain) {
			selectedDomain = (ICredentialDomain)selected;
		} else if( selected instanceof CredentialUser) {
			selectedDomain = ((CredentialUser)selected).domain;
		}
		
		NewCredentialUserDialog dialog = new NewCredentialUserDialog(getShell(), CredentialService.getCredentialModel(), selectedDomain);
		if( dialog.open() == Window.OK) {
			ICredentialDomain cd = dialog.getDomain();
			String name = dialog.getUser();
			String pass = dialog.getPass();
			if( dialog.isAlwaysPrompt()) {
				CredentialService.getCredentialModel().addPromptedCredentials(cd, name);
			} else {
				CredentialService.getCredentialModel().addCredentials(cd, name, pass);
			}
			CredentialService.getCredentialModel().save();
			tv.refresh();
		}

	}

	private void removeUserPressed() {
		IStructuredSelection ss = (IStructuredSelection)tv.getSelection();
		Object selected = ss.getFirstElement();
		if( selected instanceof CredentialUser) {
			if(!CredentialService.getCredentialModel().save()) {
				//Is not able to save model, because user does not authenticated sequre storage. 
				//Do not complete remove.
				MessageDialog.openWarning(removeUserButton.getShell(), 
						CredentialMessages.Warning, 
						CredentialMessages.UnableToDeleteCredentials);
				return;
			}
			CredentialService.getCredentialModel().removeCredentials(
					((CredentialUser)selected).domain, ((CredentialUser)selected).user);
			CredentialService.getCredentialModel().save();
			tv.refresh();
		}
	}
	
}
