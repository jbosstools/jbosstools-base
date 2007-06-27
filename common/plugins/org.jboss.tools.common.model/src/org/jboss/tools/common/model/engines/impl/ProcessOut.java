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
package org.jboss.tools.common.model.engines.impl;

import java.io.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;

public class ProcessOut implements Runnable {
    Process p = null;
    InputStream is = null;
    Writer writer = null;
    public ProcessOut(Process p, boolean error, Writer writer) {
        this.p = p;
        is = (error) ? p.getErrorStream() : p.getInputStream();
        this.writer = writer;
        new Thread(new Notifier()).start();
        new Thread(this).start();
    }

    private boolean isAlive() {
        try {
            if(is != null && is.available() > 0) return true;
        } catch (Exception e) {
            return fireDead();
        }
        return p != null || fireDead();
    }

    private boolean fireDead() {
        synchronized(waitMonitor) {
            try {
            	waitMonitor.notifyAll();
            } catch (Exception e) {
            	//ignore
            }
            is = null;
            return false;
        }
    }

    public void run() {
        byte[] b = new byte[4096];
        while(isAlive()) {
            int av = -1;
            try {
                synchronized (waitMonitor) {
                    if(!isAlive()) return;
                    if((av = is.available()) < 1) {
                        try {
                        	Thread.sleep(200);
                        } catch (Exception e) {
                        	//ignore
                        }
                        continue;
                    }
                    if(av > b.length) av = b.length;
                    av = is.read(b, 0, av);
                }
                writer.write(new String(b, 0, av));
            } catch (Exception e) {
            	ModelPlugin.getPluginLog().logError(e);
            }
        }
    }

    Object waitMonitor = new Object();

    public void waitFor() {
        if(!isAlive()) return;
        synchronized(waitMonitor) {
            if(!isAlive()) return;
            try {
            	wait();
            } catch (Exception e) {
            	//ignore
            }
        }
    }

    class Notifier implements Runnable {
        public void run() {
            try {
                p.waitFor();
            } catch (Exception e) {
                try {
                    p.exitValue();
                } catch (Exception e2) {
                    run();
                }
            }
            p = null;
        }
    }

}

