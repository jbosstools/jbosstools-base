/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.filesystems.impl;

import java.util.HashMap;
import java.util.Map;

import org.jboss.tools.common.model.XModelObject;

/**
 * This factory provides a new ready to be loaded copy of delegated model object in case when 
 * the delegated model object is being loaded by another thread, and there is a danger of deadlock
 * in waiting for completion of its loading. If the delegate is loaded, factory returns null.
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class ThreadSafeCopyFactory {
	XModelObject object;

	Thread loadingThread = Thread.currentThread();
	Map<Thread, XModelObject> concurrentlyLoaded = null;

	public ThreadSafeCopyFactory(XModelObject object) {
		this.object = object;
	}

    /**
     * Returns ready to be loaded copy of model object if and only if
     * this object is being loaded by another thread.
     * Otherwise, returns null.
     * 
     * @return
     */
    XModelObject getThreadSafeCopy() {
    	if(loadingThread == null) {
    		concurrentlyLoaded = null;
    		return null;
    	}
    	if(Thread.currentThread() == loadingThread) {
    		return null;
    	}

    	Map<Thread, XModelObject> map = concurrentlyLoaded;
    	XModelObject copy = null;
		if(map != null) {
			copy = map.get(Thread.currentThread());
			if(copy != null) return copy;
		}
		if(object.getParent() instanceof JarFolderImpl) {
			copy = ((JarFolderImpl)object.getParent()).createValidChildCopy(object);
		} else if(object.getParent() instanceof FolderImpl) {
			copy = ((FolderImpl)object.getParent()).createValidChildCopy(object);
		}
		if(copy != null) {
//			System.out.println("Created copy of " + copy.getPath() + " for thread " + Thread.currentThread() + ". Main object is being loaded by thread " + loadingThread);
			//Let us wait a bit for this object, maybe there is no lock.
			for (int i = 0; i < 50; i++) {
				Thread t = loadingThread;
				if (t != null) try {
					t.join(100);
				} catch (InterruptedException e) {
					//ignore
				}
				if(loadingThread == null) {
//					System.out.println("Drop copy of " + FileAnyImpl.toFileName(copy) + " for thread " + Thread.currentThread() + ". Main object is loaded while it was built in " + (i * 100) + " ms.");
					return null;
				}
			}
			if(map == null) {
				map = new HashMap<Thread, XModelObject>();
				concurrentlyLoaded = map;
			}
			map.put(Thread.currentThread(), (AbstractExtendedXMLFileImpl)copy);
		}
    	return copy;
    }

    public void destroy() {
    	loadingThread = null;
    	concurrentlyLoaded = null;
    }

}
