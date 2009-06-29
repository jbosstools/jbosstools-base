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
import java.util.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;

public abstract class XProcess {
    protected Process process = null;
    ProcessOut err = null;
    ProcessOut out = null;
    Writer w = new XPWriter();
    int exit = -1;

    public XProcess() {}

    private String[] getCommandLine() {
        ArrayList<String> l = new ArrayList<String>();
        buildCommandLine(l);
        return l.toArray(new String[l.size()]);
    }

    protected abstract void buildCommandLine(ArrayList<String> l);

    protected abstract String getRoot();

    public void start() {
        if(isRunning()) return;
        exit = -1;
        try {
            String[] command_line = getCommandLine();
            process = Runtime.getRuntime().exec(command_line, null, new File(getRoot()));
            err = new ProcessOut(process, true, w);
            out = new ProcessOut(process, false, w);
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        new HookMonitor();
    }

/*
    private void debugCommandLine(String[] command_line) {
    	write("Command line:\n");
    	for (int i = 0; i < command_line.length; i++) write(command_line[i] + "\n");    	
    }
*/

    public final void stop() {
        if(!isRunning()) return;
       	process.destroy(); 
       	waitFor();
        clear();
    }

    public final boolean isRunning() {
        if(process == null) return false;
        try {
            exit = process.exitValue();
            clear();
            return false;
        } catch (IllegalThreadStateException e) {
            return true;
        }
    }

    private void clear() {
        process = null;
        err = null;
        out = null;
    }

    public final int waitFor() {
        if(process == null) return exit;
        ProcessOut errc = err, outc = out;
        try {
            exit = process.waitFor();
            if(errc != null) errc.waitFor();
            if(outc != null) outc.waitFor();
            return exit;
        } catch (InterruptedException e) {
            return exit;
        }
    }

    protected void write(String s) {}

    class XPWriter extends Writer {
        public void write(char cbuf[], int off, int len) throws IOException {
            XProcess.this.write(new String(cbuf, off, len));
        }
        public void flush() throws IOException {}
        public void close() throws IOException {}
    }

    protected void appendJava(ArrayList<String> l, Properties p) {
        l.add(getJavaHome(p) + "/bin/java"); //$NON-NLS-1$
    }

    protected String getJavaHome(Properties p) {
        File f = new File(p.getProperty("java.home")); //$NON-NLS-1$
        return f.getAbsolutePath().replace('\\', '/');
////        return f.getParent().replace('\\', '/');
    }

    private class SD implements Runnable {
        public void run() {
            stop();
        }
    }

    private class HookMonitor implements Runnable {
        Thread sdhook = null;

        public HookMonitor() {
            if(!isRunning()) return;
            sdhook = new Thread(new SD());
            Runtime.getRuntime().addShutdownHook(sdhook);
            new Thread(this).start();
        }

        public void run() {
        	try {
        		process.waitFor();
        	} catch (InterruptedException e) {
        		//do nothing 
        	}
            stop();
            Runtime.getRuntime().removeShutdownHook(sdhook);
        }
    }
}
