package fi.vm.sade.service.valintalaskenta.service.test.app;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.apache.commons.io.IOUtils;

import com.sun.xml.messaging.saaj.packaging.mime.util.BASE64DecoderStream;

import de.flapdoodle.embed.mongo.MongodExecutable;
import de.flapdoodle.embed.mongo.MongodStarter;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Timeout;
import de.flapdoodle.embed.mongo.config.MongodConfig;
import de.flapdoodle.embed.mongo.config.RuntimeConfig;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Net;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Storage;
import de.flapdoodle.embed.mongo.distribution.Version;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Embedded Mongo Desktop for manual testing purposes!
 * 
 */
public class StartEmbeddedMongo {

    private static final int PORT = 37200;
    private static final String HOST = "localhost";

    public static void main(String[] args) throws IOException {
        BASE64DecoderStream b0 = new BASE64DecoderStream(new ByteArrayInputStream(ICON.getBytes()));
        ImageIcon imageIcon = new ImageIcon(IOUtils.toByteArray(b0));
        JFrame frame = new JFrame("MongoDB");
        frame.setIconImage(imageIcon.getImage());
        frame.setAlwaysOnTop(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(200, 200);
        frame.setVisible(true);

        RuntimeConfig runtimeConfig = new RuntimeConfig();
        MongodStarter runtime = MongodStarter.getInstance(runtimeConfig);
        MongodConfig config = new MongodConfig(Version.Main.V2_0, new Net(HOST, PORT, false), new Storage(),
                new Timeout());
        final MongodExecutable mongodExe = runtime.prepare(config);
        try {
            mongodExe.start();
        } catch (IOException e1) {
        }

        final Thread mainThread = Thread.currentThread();
        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                System.out.println("Stopping Embedded MongoDB!");
                mongodExe.stop();
                System.out.println("Done!");
                try {
                    mainThread.join();
                } catch (InterruptedException e) {
                }
            }
        });

    }
    
    private static final String ICON = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBI"
            + "WXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH3QISDB0G/bFZaAAAAB1pVFh0Q29tbWVudAAAAAAAQ3Jl"
            + "YXRlZCB3aXRoIEdJTVBkLmUHAAAFuUlEQVRYw72Xf2yV1RnHP89571V+FK2isJIKipgME4mBCrQM"
            + "ZZqI+INEEickBIy6kv0xhu5HxlZBdMkyWBgrJKjgD8SK4Q+MkoVAosgS6VZKyUIWF6UUtBSLCUNp"
            + "MuDe8zz745z3vb2lRarRc/Pc95zz3vv8/H6f874wiLFo12PZfPGuJ1besmFGNcAt6+v4pkMG+4fF"
            + "ux6/LUmSt9TrrS3HPjl/oVCYeGRpc8fN62fQ/vMPB+1A7lI3H9rxMDvnvc0jb81PGCaL8kmyTE0n"
            + "iQkIOJErxbmjExpnrMHYCHRMaKzFgPalzZflgOtv8+7tDwCwc97bzN0x747CUH8G9BVvOknNo+pR"
            + "M0wFUwB+DRyd0Fj3wpGlzVAIiR2/rm7wJbhr22z2LdjNvdvnXiPOmnIuNyfnHIkkJC4hEUfO5RAc"
            + "bcePcq5QACtT0QOsaf/F/ucGnYGpb9zNvgW7mbnt3tqCFU6bMUdNUVO8aYxc8eYxNERvF+msAFbd"
            + "vK7u+Pi/1o0Lmaj9egcmb51Fy8L3md50z88M9psZ3jyqhjfDTFGM1CGLuTcbQGAsxkfj19U+eXRZ"
            + "M+PX9u9EAvDD12dyeNHfuX3rrKdEpNFlxREk/U7Xkq6F7i+/pKgaVVlZXaPkMeZWzr6hs+Pp5kM3"
            + "rZ3Omd2d/WNg4paZs3Iu2XuFCIk48uIItXckLiGX1T9ckyTH4WOdAQMDDSurz7MdT/9jVb8ZGLdl"
            + "xtWC+08arcSAJUaNgMOFddhEgC/OnMV73zviiwSLE2Nm5ezqXWf2dHZd3Ac872hieMCZ4szhBcQM"
            + "QREDjwdLENOoz2EYapfR7cJvEjFa+jLPVW2uvclDjTfFm1HEKFoAXipqFlmgZWssKO8tlElApEVR"
            + "jHF/nvZmmQNebLKaDvcK3lKJTlAyrKYoqRPhmirtLeUf0F7MiA7ef+OaqWMzB9TsJx5Qs4zvwXAU"
            + "gqgZPjVuhtI3A5atMbC4TmGggA/zqxUZecPqqZkDtymGJ00vmeGiltIdHIuZ0OCEj4qDcsnmFmFq"
            + "ZTiw6KRhZpM++01LAKHCFaIgLqQ/gM5FFhhFC8hPFXgUEcGpkBO4YFbG5wz0Vtq0EhBTdl5fKgF8"
            + "rBHNIULwGqNLQaixFGnk6gMInZRqrEFQw9JNDSJa3iYFO5w5YGZNGmseUlkyVozzjBkQwWioalCW"
            + "Ks6g37cv9iEGqEJn9R9rQgnM7D1FTovYtWagAqLgneDMKCJZWRKnYa3gE4n06s/kQDsAHAM90rm8"
            + "LWSgZ0nr52a2QelFQymlP4CPmAniPNwvFvWiyL9ueM+zJ5a3nS87DXuWHFipao3hiA0MTqmYnoYZ"
            + "DTU6pUEGPA37l9UnG1q3XreypvwwqnixhmKRJJ93TU54NBEhESFHvIoj54ScSDik4rX7xNmYhZIy"
            + "Sw+heGaYBUIa7HGmD5pLCid+10qfDLSSy4k/W98y38x+G/AVQae9WzKxJ0BRS90wa7UWGBBYEeYS"
            + "7jV1/f7gbMWVGS97IOlZcoARL03lq/oDf1KxH3vjUzULeIidMLAiOqGGswDIgc8CAOq7GtoWjvnD"
            + "ZLoaDl76kexsfQsjNt1BXpMP/vvTf44zs1cV+5/XSEMttechmjDU5yMge0mw6xUOXYCKEw0HN1U9"
            + "P4WuhrZv9l5w3abp1c7JsgR+mWLBiTBFR3H6VA//Pv9FX/J9KMiyrhVtrQA/eH4Knz9zcHCP5b3H"
            + "nDG3d556ovlX6mWIxzZ6tY5zFJlWHMPjVdMYO3QkTtwpg50KNSdXHPpRahy4pPHLfjMavbmW7ifj"
            + "i8Zr1fnlF+5bMDE/asvQYRUMG15xaH373jtPftXd869n9jP6ucl0r2jjOxt/eWkpANuaVrX/7d1G"
            + "2/f+6/O/jT432D88Vd8IwPDhV71QMaKSa64ddfx7dSAdIyoqP6qsvJ7RVWOr+L7He3tezubnzp0a"
            + "8m10/R+/9h8aeaqFEgAAAABJRU5ErkJggg==";

}
