/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.jboss.tools.foundation.security;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.crypto.spec.PBEKeySpec;
import org.eclipse.core.runtime.Path;
import org.eclipse.equinox.security.storage.EncodingUtils;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
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

/**
 *
 */
public class DescriptiveStorageLoginDialog extends FormDialog {

    private static final String HELP_ID = "org.jboss.tools.foundation.security.linux.sec_storage_context"; //$NON-NLS-1$

    private static final String DIGEST_ALGORITHM = "MD5"; //$NON-NLS-1$

    protected Text password;
    protected Text confirm;
    private Label statusLabel;

    protected Button showPassword;
    protected Button okButton;

    protected PBEKeySpec generatedPassword;

    protected final boolean confirmPassword;
    protected final boolean passwordChange;
    protected final String location;

    /**
     * Create new dialog instance
     *
     * @param confirmPassword
     * @param passwordChange
     * @param location
     */
    public DescriptiveStorageLoginDialog(boolean confirmPassword, boolean passwordChange, String location) {
        super(getWorkbenchShell());
        this.confirmPassword = confirmPassword;
        this.passwordChange = passwordChange;
        this.location = location;
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
    public PBEKeySpec getGeneratedPassword() {
        return generatedPassword;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID, Messages.ButtonLogin, true);
        okButton.setEnabled(false);
        createButton(parent, IDialogConstants.CANCEL_ID, Messages.ButtonExit, false);
    }

    @Override
    protected boolean isResizable() {
        return false;
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText(Messages.DialogTitle);

        PlatformUI.getWorkbench().getHelpSystem().setHelp(shell, HELP_ID);

        shell.setSize(700, 500);
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
        descriptionSection.setText(Messages.DescriptionTitle);
        descriptionSection.setTitleBarForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));

        FormText descriptionText = toolkit.createFormText(descriptionSection, true);
        descriptionText.setText(Messages.SecureDescription, true, true);
        TableWrapData td = new TableWrapData(TableWrapData.FILL);
        td.colspan = 1;
        descriptionText.setLayoutData(td);

        descriptionSection.setClient(descriptionText);

        /* password panel */
        String titleMsg;
        if (confirmPassword)
            titleMsg = Messages.PasswordChangeTitle;
        else if (passwordChange)
            titleMsg = Messages.MessageLoginChange;
        else
            titleMsg = Messages.DialogTitle;

        /* Separator */
        toolkit.createLabel(body, ""); //$NON-NLS-1$

        Section passwordSection = toolkit.createSection(body, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
        passwordSection.setText(titleMsg);
        passwordSection.setTitleBarForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        passwordSection.setLayoutData(td);

        Composite passwordComposite = toolkit.createComposite(passwordSection, SWT.NONE);
        GridLayoutFactory.fillDefaults().numColumns(2).applyTo(passwordComposite);

        Label passwordLabel = toolkit.createLabel(passwordComposite, Messages.LabelPassword, SWT.LEFT);
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

        if (confirmPassword) {
            Label passwordConfLabel = toolkit.createLabel(passwordComposite, Messages.LabelConfirm, SWT.LEFT);
            GridDataFactory.swtDefaults().applyTo(passwordConfLabel);

            confirm = toolkit.createText(passwordComposite, "", SWT.LEFT | SWT.BORDER); //$NON-NLS-1$
            GridDataFactory.fillDefaults().grab(true, false).applyTo(confirm);

            confirm.addModifyListener(new ModifyListener() {
                public void modifyText(ModifyEvent event) {
                    okButton.setEnabled(validatePassword(mainForm));
                }
            });
        } else confirm = null;

        Label fillerLabel = toolkit.createLabel(passwordComposite, "", SWT.LEFT);// filler //$NON-NLS-1$
        GridDataFactory.swtDefaults().applyTo(fillerLabel);

        showPassword = toolkit.createButton(passwordComposite, Messages.ShowPassword, SWT.CHECK | SWT.RIGHT);
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

        if (location != null) {
            Group locationGroup = new Group(passwordComposite, SWT.NONE);
            locationGroup.setText(Messages.LocationGroup);
            toolkit.adapt(locationGroup);
            GridDataFactory.fillDefaults().grab(true, true).span(2, 1).applyTo(locationGroup);
            GridLayoutFactory.fillDefaults().applyTo(locationGroup);

            Label locationLabel = toolkit.createLabel(locationGroup, new Path(location).toOSString(), SWT.WRAP);
            GridDataFactory.fillDefaults().grab(true, false).applyTo(locationLabel);
        }

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
            if (confirm != null)
                confirm.setEchoChar('\0');
        } else {
            password.setEchoChar('*');
            if (confirm != null)
                confirm.setEchoChar('*');
        }
    }

    protected boolean validatePassword(ScrolledForm mainForm) {
        String password1 = password.getText();
        if ((password1 == null) || (password1.length() == 0)) {
            statusLabel.setText(Messages.MessageEmptyPassword);
            return false;
        }
        if (confirm != null) {
            String password2 = confirm.getText();
            if (!password1.equals(password2)) {
                statusLabel.setText(Messages.MessageNoMatch);
                return false;
            }
        }
        statusLabel.setText(""); //$NON-NLS-1$
        return true;
    }

    @Override
    protected void okPressed() {
        String internalPassword;
        try {
            // normally use digest of what was entered
            MessageDigest digest = MessageDigest.getInstance(DIGEST_ALGORITHM);
            byte[] digested = digest.digest(new String(password.getText()).getBytes());
            internalPassword = EncodingUtils.encodeBase64(digested);
        } catch (NoSuchAlgorithmException e) {
            // just use the text as is
            internalPassword = password.getText();
        }
        generatedPassword = new PBEKeySpec(internalPassword.toCharArray());

        super.okPressed();
    }
}
