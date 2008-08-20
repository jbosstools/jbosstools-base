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


package org.jboss.tools.common.model.ui.dnd;


import java.util.Properties;

import org.eclipse.swt.events.TypedEvent;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.editors.dnd.context.DropContext;
import org.jboss.tools.common.model.ui.editors.dnd.context.IDNDTextEditor;
import org.jboss.tools.vpe.xulrunner.XPCOM;
import org.mozilla.interfaces.nsIComponentManager;
import org.mozilla.interfaces.nsIDOMEvent;
import org.mozilla.interfaces.nsIDragService;
import org.mozilla.interfaces.nsIDragSession;
import org.mozilla.interfaces.nsIFile;
import org.mozilla.interfaces.nsIServiceManager;
import org.mozilla.interfaces.nsISupports;
import org.mozilla.interfaces.nsITransferable;
import org.mozilla.xpcom.Mozilla;


/**
 * The Class DnDUtil.
 */
public class DnDUtil {

    /** The Constant kTextMime. */
    public static final String kTextMime = "text/plain"; //$NON-NLS-1$

    /** The Constant kUnicodeMime. */
    public static final String kUnicodeMime = "text/unicode"; //$NON-NLS-1$

    /** The Constant kHTMLMime. */
    public static final String kHTMLMime = "text/html"; //$NON-NLS-1$

    /** The Constant kAOLMailMime. */
    public static final String kAOLMailMime = "AOLMAIL"; //$NON-NLS-1$

    /** The Constant kPNGImageMime. */
    public static final String kPNGImageMime = "image/png"; //$NON-NLS-1$

    /** The Constant kJPEGImageMime. */
    public static final String kJPEGImageMime = "image/jpg"; //$NON-NLS-1$

    /** The Constant kGIFImageMime. */
    public static final String kGIFImageMime = "image/gif"; //$NON-NLS-1$

    /** The Constant kFileMime. */
    public static final String kFileMime = "application/x-moz-file"; //$NON-NLS-1$

    /** The Constant kURLMime. */
    public static final String kURLMime = "text/x-moz-url"; //$NON-NLS-1$

    /** The Constant kURLDataMime. */
    public static final String kURLDataMime = "text/x-moz-url-data"; //$NON-NLS-1$

    /** The Constant kURLDescriptionMime. */
    public static final String kURLDescriptionMime = "text/x-moz-url-desc"; //$NON-NLS-1$

    /** The Constant kNativeImageMime. */
    public static final String kNativeImageMime = "application/x-moz-nativeimage"; //$NON-NLS-1$

    /** The Constant kNativeHTMLMime. */
    public static final String kNativeHTMLMime = "application/x-moz-nativehtml"; //$NON-NLS-1$

    /** The Constant kFilePromiseURLMime. */
    public static final String kFilePromiseURLMime = "application/x-moz-file-promise-url"; //$NON-NLS-1$

    /** The Constant kFilePromiseMime. */
    public static final String kFilePromiseMime = "application/x-moz-file-promise"; //$NON-NLS-1$

    /** The Constant kFilePromiseDirectoryMime. */
    public static final String kFilePromiseDirectoryMime = "application/x-moz-file-promise-dir"; //$NON-NLS-1$

    /** The Constant FLAVORS. */
    public static final String[] FLAVORS = { kTextMime, kUnicodeMime, kHTMLMime, kAOLMailMime, kPNGImageMime, kJPEGImageMime,
            kGIFImageMime, kFileMime, kURLMime, kURLDataMime, kURLDescriptionMime, kNativeImageMime, kNativeHTMLMime, kFilePromiseURLMime,
            kFilePromiseMime, kFilePromiseDirectoryMime };

    /**
     * Gets the enabled action.
     * 
     * @param actionpath the actionpath
     * @param object the object
     * @param targets the targets
     * 
     * @return the enabled action
     */
    public static XAction getEnabledAction(XModelObject object, XModelObject[] targets, String actionpath) {
        if (object == null)
            return null;
        XAction action = object.getModelEntity().getActionList().getAction(actionpath);
        if (action == null)
            return null;
        return (targets == null) ? ((!action.isEnabled(object)) ? null : action) : ((!action.isEnabled(object, targets)) ? null : action);
    }

    /**
     * Checks if is copy enabled.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return true, if is copy enabled
     */
    public static boolean isCopyEnabled(XModelObject object, XModelObject[] targets) {
        return getEnabledCopyAction(object, targets) != null;
    }

    /**
     * Gets the enabled copy action.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return the enabled copy action
     */
    public static XAction getEnabledCopyAction(XModelObject object, XModelObject[] targets) {
        return getEnabledAction(object, targets, "CopyActions.Copy"); //$NON-NLS-1$
    }

    /**
     * Copy.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return true, if copy
     */
    public static boolean copy(XModelObject object, XModelObject[] targets) {
        XAction copy = getEnabledCopyAction(object, targets);
        if (copy == null)
            return false;
        try {
            Properties p = new Properties();
            p.setProperty("isDrag", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            if (targets == null) {
                copy.executeHandler(object, p);
            } else {
                copy.executeHandler(object, targets, p);
            }
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Checks if is paste enabled.
     * 
     * @param object the object
     * 
     * @return true, if is paste enabled
     */
    public static boolean isPasteEnabled(XModelObject object) {
        return getEnabledPasteAction(object) != null;
    }

    /**
     * Gets the enabled paste action.
     * 
     * @param object the object
     * 
     * @return the enabled paste action
     */
    public static XAction getEnabledPasteAction(XModelObject object) {
        XAction action = getEnabledAction(object, null, "CopyActions.Paste"); //$NON-NLS-1$
        return (action != null) ? action : getEnabledAction(object, null, "MoveActions.Move"); //$NON-NLS-1$
    }

    /**
     * Paste.
     * 
     * @param object the object
     * @param properties the properties
     * 
     * @throws XModelException the X model exception
     */
    public static void paste(XModelObject object, Properties properties) throws XModelException {
        XAction paste = getEnabledPasteAction(object);
        if (paste != null)
            paste.executeHandler(object, properties);
    }

    /**
     * Convert2 vpe dn D event.
     * 
     * @param event the event
     * 
     * @return the vpe dn D event
     */
    public static VpeDnDEvent convert2VpeDnDEvent(nsIDOMEvent event) {
        return null;

    }

    /**
     * Fire dn D event.
     * 
     * @param dropContext the drop context
     * @param event the event
     * @param textEditor the text editor
     */
    public static void fireDnDEvent(DropContext dropContext, IDNDTextEditor textEditor, TypedEvent event) {

        dropContext.runDropCommand(textEditor, event);
    }

    /**
     * Gets the dn D file.
     * 
     * @param event the event
     * 
     * @return the dn D file
     */
    public static nsIFile getDnDFile(nsIDOMEvent event) {
        nsIFile aFile = null;
        nsIServiceManager serviceManager = Mozilla.getInstance().getServiceManager();
        nsIComponentManager componentManager = Mozilla.getInstance().getComponentManager();
        nsIDragService dragService = (nsIDragService) serviceManager.getServiceByContractID("@mozilla.org/widget/dragservice;1", //$NON-NLS-1$
                nsIDragService.NS_IDRAGSERVICE_IID);
        final nsIDragSession dragSession = dragService.getCurrentSession();

        final nsITransferable iTransferable = (nsITransferable) componentManager.createInstanceByContractID(
                XPCOM.NS_TRANSFERABLE_CONTRACTID, null, nsITransferable.NS_ITRANSFERABLE_IID);

        for (String flavor1 : FLAVORS) {
            if (dragSession.isDataFlavorSupported(flavor1)) {
                iTransferable.addDataFlavor(flavor1);
                System.out.println("\n->" + flavor1); //$NON-NLS-1$
            }
        }
        String[] aFlavor = { "" }; //$NON-NLS-1$
        long[] aDataLen = { 0 };
        nsISupports[] aData = { null };

        dragSession.getData(iTransferable, 0);

        iTransferable.getAnyTransferData(aFlavor, aData, aDataLen);
        System.out.println("aData[0] className = " + aData[0].getClass().getName()); //$NON-NLS-1$

        
        aFile = (nsIFile) aData[0].queryInterface(nsIFile.NS_IFILE_IID);
        return aFile;
    }

}
