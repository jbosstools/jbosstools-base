/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.ui.credentials.internal;


import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.FormDialog;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.jboss.tools.foundation.core.credentials.ICredentialDomain;

/**
 *  This class is a stripped-down clone of 
 *  org.jboss.tools.foundation.security.DescriptiveStorageLoginDialog
 *  The two could possibly be unified at one point in the future if overall 
 *  architecture allows it. 
 */
public class CredentialPromptDialog extends FormDialog {
    protected Text password;
    private Label statusLabel;

    protected Button showPassword;
    protected Button okButton;

    protected String providedPassword;
    private ICredentialDomain domain;
    private String user;
    
    /**
     * Create new dialog instance
     *
     * @param confirmPassword
     * @param passwordChange
     * @param location
     */
    public CredentialPromptDialog(ICredentialDomain domain, String user) {
        super(getWorkbenchShell());
        this.domain = domain;
        this.user = user;
    }

    /**
     * Get the shell from an active window. If not found, returns null.
     */
    private static Shell getWorkbenchShell() {
        if (PlatformUI.isWorkbenchRunning()) {
            IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
            if (window != null)
                return window.getShell();
        }
        return null;
    }

    /**
     * Get the generated password
     *
     * @return {@link PBEKeySpec} instance of password
     */
    public String getPassword() {
        return providedPassword;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID, CredentialMessages.OK, true);
        okButton.setEnabled(false);
        createButton(parent, IDialogConstants.CANCEL_ID, CredentialMessages.Cancel, false);
    }

    @Override
    protected boolean isResizable() {
        return false;
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText(CredentialMessages.CredentialPrompterTitle);
        shell.setSize(700, 250);
    }

    @Override
    protected void createFormContent(IManagedForm mform) {
        final ScrolledForm mainForm = mform.getForm();
        FormToolkit toolkit = new FormToolkit(mainForm.getDisplay());

        Composite body = mainForm.getBody();
        TableWrapLayout layout = new TableWrapLayout();
        body.setLayout(layout);

        /* description panel */
        Section descriptionSection = toolkit.createSection(body, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
        descriptionSection.setText(CredentialMessages.DescriptionSectionTitle);
        descriptionSection.setTitleBarForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));

        FormText descriptionText = toolkit.createFormText(descriptionSection, true);
        descriptionText.setText(NLS.bind(CredentialMessages.DescriptionSectionContent, user, domain.getName()), true, true);
        TableWrapData td = new TableWrapData(TableWrapData.FILL);
        td.colspan = 1;
        descriptionText.setLayoutData(td);

        descriptionSection.setClient(descriptionText);


        /* Separator */
        toolkit.createLabel(body, ""); //$NON-NLS-1$

        Section passwordSection = toolkit.createSection(body, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
        passwordSection.setText("");
        passwordSection.setTitleBarForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        passwordSection.setLayoutData(td);

        Composite passwordComposite = toolkit.createComposite(passwordSection, SWT.NONE);
        GridLayoutFactory.fillDefaults().numColumns(2).applyTo(passwordComposite);

        Label passwordLabel = toolkit.createLabel(passwordComposite, CredentialMessages.PasswordLabel2, SWT.LEFT);
        GridDataFactory.swtDefaults().applyTo(passwordLabel);

        password = toolkit.createText(passwordComposite, "", SWT.LEFT | SWT.BORDER); //$NON-NLS-1$
        GridDataFactory.fillDefaults().grab(true, false).applyTo(password);

        password.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                okButton.setEnabled(validatePassword(mainForm));
            }
        });

        /* This textbox has the focus on dialog display */
        password.setFocus();
        Label fillerLabel = toolkit.createLabel(passwordComposite, "", SWT.LEFT);// filler //$NON-NLS-1$
        GridDataFactory.swtDefaults().applyTo(fillerLabel);

        showPassword = toolkit.createButton(passwordComposite, CredentialMessages.ShowPasswordLabel, SWT.CHECK | SWT.RIGHT);
        GridDataFactory.fillDefaults().align(SWT.RIGHT, SWT.CENTER).applyTo(showPassword);

        showPassword.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                passwordVisibility();
            }

            public void widgetDefaultSelected(SelectionEvent e) {
                passwordVisibility();
            }
        });

        // by default don't display password as clear text
        showPassword.setSelection(false);
        passwordVisibility();
        passwordSection.setClient(passwordComposite);

        /* Separator */
        toolkit.createLabel(body, ""); //$NON-NLS-1$

        /* Status label for any messages */
        statusLabel = toolkit.createLabel(body, ""); //$NON-NLS-1$
        TableWrapData td2 = new TableWrapData(TableWrapData.FILL);
        td2.colspan = 1;
        td2.grabHorizontal = true;
        statusLabel.setLayoutData(td2);
        statusLabel.setForeground(mainForm.getDisplay().getSystemColor(SWT.COLOR_RED));
    }

    protected void passwordVisibility() {
        boolean selected = showPassword.getSelection();
        if (selected) {
            password.setEchoChar('\0');
        } else {
            password.setEchoChar('*');
        }
    }

    protected boolean validatePassword(ScrolledForm mainForm) {
        String password1 = password.getText();
        if ((password1 == null) || (password1.length() == 0)) {
            statusLabel.setText(CredentialMessages.MessageEmptyPassword);
            return false;
        }
        statusLabel.setText(""); //$NON-NLS-1$
        return true;
    }

    @Override
    protected void okPressed() {
    	providedPassword = password.getText();
        super.okPressed();
    }
}
