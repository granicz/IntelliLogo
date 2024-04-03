import { defineConfig } from 'vite';
import basicSsl from '@vitejs/plugin-basic-ssl'
import http from 'https';

export default defineConfig({
    plugins: [basicSsl()],
  
    build: {
         outDir: "../../dist"
    },
})