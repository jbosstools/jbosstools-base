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

import java.io.*;
import org.eclipse.swt.dnd.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ModelTransfer extends ByteArrayTransfer {
	public static final String MODEL = "vpe/model";
	public static final int MODEL_ID = registerType(MODEL);

	private static ModelTransfer instance = new ModelTransfer();
	
	public static ModelTransfer getInstance() {
		return instance;
	}

	protected int[] getTypeIds() {
		return new int[] {MODEL_ID};
	}

	protected String[] getTypeNames() {
		return new String[]{MODEL};
	}

	protected void javaToNative (Object data, TransferData transferData){
		if (data == null) return;
		try {
			String realData = (String)data;
///			String[] realData = (String[])data;
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			DataOutputStream dataOut = new DataOutputStream(out);
			/*
			dataOut.writeInt(realData.length);
			for (int i = 0; i < realData.length; i++) {
				dataOut.writeUTF(realData[i]);
			}
			*/
			dataOut.writeUTF(realData);
			dataOut.close();
			super.javaToNative(out.toByteArray(), transferData);
		} catch (IOException e) {
			ModelUIPlugin.log(e);
		}	
	}
	protected Object nativeToJava(TransferData transferData) {
		try {
			byte[] bytes = (byte[]) super.nativeToJava(transferData);
			ByteArrayInputStream in = new ByteArrayInputStream(bytes);
			DataInputStream dataIn = new DataInputStream(in);
			/*
			int len = dataIn.readInt();
			String[] data = new String[len];
			for (int i = 0; i < len; i++) {
				data[i] = dataIn.readUTF();
			}
			*/
			String data = dataIn.readUTF();
			return data;
		} catch (IOException e) {
			ModelUIPlugin.log(e);
		}
		//can't get here
		return null;
	}
}