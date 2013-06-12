/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.jboss.tools.foundation.security;

import java.net.URL;
import javax.crypto.spec.PBEKeySpec;
import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.provider.IPreferencesContainer;
import org.eclipse.equinox.security.storage.provider.IProviderHints;
import org.eclipse.equinox.security.storage.provider.PasswordProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;

/**
 * Password provider that display a much more descriptive dialog
 * for password entry.
 */
public class DescriptiveDialogPasswordProvider extends PasswordProvider {

    /*
     * Taken from org.eclipse.equinox.internal.security.storage.friends.InternalExchangeUtils
     */
    private static final String JUNIT_APPS1 = "org.eclipse.pde.junit.runtime."; //$NON-NLS-1$

    /*
     * Taken from org.eclipse.equinox.internal.security.storage.friends.InternalExchangeUtils
     */
    private static final String JUNIT_APPS2 = "org.eclipse.test."; //$NON-NLS-1$

    @Override
    /**
     * Original code taken from org.eclipse.equinox.internal.security.storage.DefaultPassword
     * then modified appropriately.
     */
    public PBEKeySpec getPassword(IPreferencesContainer container, int passwordType) {
        if (! showUI(container))
            return null;

        boolean newPassword = ((passwordType & CREATE_NEW_PASSWORD) != 0);
        boolean passwordChange = ((passwordType & PASSWORD_CHANGE) != 0);

        String location = container.getLocation().getFile();
        URL defaultURL = defaultStorageLocation();
        if (defaultURL != null) { // remove default location from the dialog
            String defaultFile = defaultURL.getFile();
            if (defaultFile != null && defaultFile.equals(location)) location = null;
        }

        final DescriptiveStorageLoginDialog loginDialog = new DescriptiveStorageLoginDialog(newPassword, passwordChange, location);

        final PBEKeySpec[] result = new PBEKeySpec[1];
        PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
            public void run() {
                if (loginDialog.open() == Window.OK) result[0] = loginDialog.getGeneratedPassword();
                else result[0] = null;
            }
        });
        return result[0];
    }

    /*
     * Taken from  org.eclipse.equinox.internal.security.storage.StorageUtils
     */
    private boolean showUI(IPreferencesContainer container) {

        if (!PlatformUI.isWorkbenchRunning() || isJUnitApp()) return false;

        if (container == null) return true;

        if (container.hasOption(IProviderHints.PROMPT_USER)) {
            Object promptHint = container.getOption(IProviderHints.PROMPT_USER);
            if (promptHint instanceof Boolean)

            return ((Boolean)promptHint).booleanValue();
        }

        return true;

    }

    /*
     * Taken from org.eclipse.equinox.internal.security.storage.friends.InternalExchangeUtils
     */
    private boolean isJUnitApp() {
        BundleContext context = FrameworkUtil.getBundle(getClass()).getBundleContext();

        if (context == null) return false;

        String app = context.getProperty("eclipse.application"); //$NON-NLS-1$
        if (app == null) return false;

        if (app.startsWith(JUNIT_APPS1)) return true;

        if (app.startsWith(JUNIT_APPS2)) return true;

        return false;
    }
    
    /**
     * Returns location of default storage
     * 
     * @return location of the default storage, might be null
     */
    private static URL defaultStorageLocation() {
        ISecurePreferences defaultStorage = SecurePreferencesFactory.getDefault();
        if (defaultStorage == null)
            return null;
        return ((SecurePreferencesWrapper) defaultStorage).getContainer().getLocation();
    }
}
