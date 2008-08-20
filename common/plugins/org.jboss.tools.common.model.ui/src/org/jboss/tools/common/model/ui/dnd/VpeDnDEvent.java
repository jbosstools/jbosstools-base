/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/


package org.jboss.tools.common.model.ui.dnd;


import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.events.TypedEvent;


/**
 * The Class VpeDnDEvent.
 * 
 * @author Evgenij Stherbin
 */
public class VpeDnDEvent extends TypedEvent {

    /** The Constant serialVersionUID. */
    private static final long serialVersionUID = 4032394561953756746L;

    /** The x-cordinate of the cursor relative to the <code>Display</code>. */
    private int x;

    /** The y-cordinate of the cursor relative to the <code>Display</code>. */
    private int y;

    /** A list of the types of data that the DragSource is capable of providing. The currentDataType must be a member of this list. */
    private TransferData[] dataTypes;

    /** The type of data that will be dropped. */
    public TransferData currentDataType;

    /**
     * Gets the current data type.
     * 
     * @return the current data type
     */
    public TransferData getCurrentDataType() {
        return currentDataType;
    }

    /**
     * Sets the current data type.
     * 
     * @param currentDataType the current data type
     */
    public void setCurrentDataType(TransferData currentDataType) {
        this.currentDataType = currentDataType;
    }

    /**
     * Gets the data types.
     * 
     * @return the data types
     */
    public TransferData[] getDataTypes() {
        return dataTypes;
    }

    /**
     * Sets the data types.
     * 
     * @param dataTypes the data types
     */
    public void setDataTypes(TransferData[] dataTypes) {
        this.dataTypes = dataTypes;
    }

    /**
     * The Constructor.
     * 
     * @param source the source
     */
    public VpeDnDEvent(Object source) {
        super(source);

    }

}
